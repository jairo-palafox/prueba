






CREATE FUNCTION "safreviv".sp_mig_mdt_reverso_inserta_recur(
                               p_folio DECIMAL(9,0),
                               p_usuario CHAR(20))
RETURNING INTEGER, CHAR(200);

DEFINE v_sql_error               INTEGER;
DEFINE v_isam_error              SMALLINT;
DEFINE v_msg_error               CHAR(200);
DEFINE v_folio_reverso           DECIMAL(9,0);

   -- Captura el error sql
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      -- Imprime el codigo de error
      --TRACE 'Ocurrio el error:'||v_sql_error;
      --TRACE 'Error ISAM:'||v_isam_error;
      --TRACE 'MSG Error:'||v_msg_error;      
      RETURN v_sql_error,v_msg_error;
   END EXCEPTION;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_mdt_inserta_inst_recurrente.trace';
   --TRACE '1';
   LET v_sql_error = 0;
   LET v_msg_error = '';
   LET v_folio_reverso = p_folio;
         
   DELETE FROM safre_viv:mdt_solicitud_mandato 
   WHERE FOLIO =  v_folio_reverso;  
          
   --TRACE '2';
   DELETE FROM safre_viv:mdt_lote_mandato
   WHERE FOLIO =  v_folio_reverso;         
                
   --TRACE '3';
   
   UPDATE safre_mig:glo_ctr_archivo a
   SET a.estado = 1
   WHERE a.proceso_cod = 1310         
   AND a.estado = 2;
   
   UPDATE safre_viv:glo_folio f
   SET f.status = -1
   WHERE f.proceso_cod = 1310         
   AND f.status =0;
   
   --TRACE '3'; 
   
   RETURN v_sql_error,v_msg_error;

END FUNCTION;


