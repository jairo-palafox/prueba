






CREATE PROCEDURE "safreviv".fn_dis_extractor(p_folio DECIMAL(9,0), p_destino SMALLINT)

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 15072015
--Declaración de variables

DEFINE v_bnd_proceso          SMALLINT;
DEFINE v_char                 CHAR(20);

DEFINE v_status               SMALLINT;
DEFINE sql_err                INTEGER ;
DEFINE isam_err               INTEGER ;
DEFINE error_info             CHAR(70);


ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION

--SET DEBUG FILE TO '/ds/safreviv_int/dis/respaldo/PRODINF-711/fn_dis_extractor.TRACE';
--TRACE ON;

--Inicialización de variables
LET v_bnd_proceso             = 0;
LET v_char                    = "";

LET v_status                  = 0;

LET sql_err                   = 0;
LET isam_err                  = 0;
LET error_info                = "";


   IF p_destino = 1 THEN  -- PAGO REAL
      EXECUTE FUNCTION fn_dis_extractor_pr(p_folio) INTO v_bnd_proceso, v_status, error_info;
   END IF

   IF p_destino = 2 THEN  -- ENTIDADES FINANCIERAS
      EXECUTE FUNCTION fn_dis_extractor_ef(p_folio) INTO v_bnd_proceso, v_status, error_info;
   END IF

   IF p_destino = 3 THEN  -- INCONSISTENCIAS
      EXECUTE FUNCTION fn_dis_extractor_in(p_folio) INTO v_bnd_proceso, v_status, error_info;
   END IF

   IF p_destino = 4 THEN  -- APORTACIONES SUBSECUENTES
      EXECUTE FUNCTION fn_dis_extractor_as(p_folio) INTO v_bnd_proceso, v_status, error_info;
   END IF

   IF p_destino = 5 THEN  -- PORTABILIDAD
      EXECUTE FUNCTION fn_dis_extractor_pt(p_folio) INTO v_bnd_proceso, v_status, error_info;
   END IF

   IF p_destino = 6 THEN  -- AVANCES COMPENSADOS
      EXECUTE FUNCTION fn_dis_extractor_ca(p_folio) INTO v_bnd_proceso, v_status, error_info;
   END IF

   LET v_char = error_info;
   
   --TRACE 'Finaliza fn_dis_extractor con valor '||v_bnd_proceso;
   
   --LET v_char = "Terminado Extractor Dispersión";
   RETURN v_bnd_proceso , v_status , v_char;

END PROCEDURE;


