






CREATE PROCEDURE "safreviv".fn_mdt_rev_inserta_inst(p_folio    SMALLINT)
                             RETURNING SMALLINT, DECIMAL(9,0);

   DEFINE v_existe                SMALLINT;
   DEFINE v_solicitudes_afectadas INTEGER;
   DEFINE v_id_solicitud_mandato  DECIMAL(9,0);

   DEFINE v_pid                   INTEGER     ;
   DEFINE v_proceso_cod           INTEGER     ;
   DEFINE v_opera_cod             INTEGER     ;
   DEFINE v_resulta               SMALLINT    ;

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_mdt_rev_inserta_inst.trace';

   --TRACE 'Inicia el store procedure de reverso de mandatos de acreditados';
   --TRACE "Parte 1 - verifica datos validos ";
  
   LET v_pid         = 0   ;
   LET v_proceso_cod = 0   ;
   LET v_opera_cod   = 0   ;

   IF p_folio = 0 OR p_folio IS NULL THEN
      RETURN 1,0;
   END IF;

   LET v_existe = 0;
   
   SELECT NVL(COUNT(*),0)
     INTO v_existe
     FROM safre_viv:mdt_lote_mandato
    WHERE folio    = p_folio
      AND estado  = 101;

   --TRACE "Parte 2 - verifica lote con estado 101 ";
   IF v_existe = 0 OR v_existe IS NULL THEN
      RETURN 1,0;
   END IF;
   
   LET v_solicitudes_afectadas = 0;
   
   FOREACH SELECT id_solicitud_mandato INTO v_id_solicitud_mandato
             FROM safre_viv:mdt_solicitud_mandato
            WHERE folio    = p_folio

      DELETE FROM safre_viv:mdt_solicitud_mandato
       WHERE id_solicitud_mandato = v_id_solicitud_mandato;
      
      IF SQLCODE = 0 THEN
         LET v_solicitudes_afectadas = v_solicitudes_afectadas + 1;
      END IF;
   
   END FOREACH;
   
   DELETE FROM safre_viv:mdt_lote_mandato
    WHERE folio    = p_folio
     AND  estado  = 101 ;


   SELECT a.pid         ,
          a.proceso_cod ,
          a.opera_cod   
   INTO   v_pid         ,
          v_proceso_cod ,
          v_opera_cod   
   FROM   bat_ctr_operacion a
   WHERE  a.proceso_cod = 1303  -- validacion de recurrente
   AND    a.opera_cod   = 2     -- integracion
   AND    a.folio       = p_folio;

   EXECUTE FUNCTION fn_reversa_operacion(v_pid,v_proceso_cod,v_opera_cod) 
   INTO    v_resulta;

   --TRACE "Fin y reverso del SP";
   RETURN 0,v_solicitudes_afectadas;

END PROCEDURE;


