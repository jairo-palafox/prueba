






CREATE FUNCTION "safreviv".fn_glo_valida_marcaje(p_nss   CHAR(11),
                                      p_marca SMALLINT)
RETURNING SMALLINT,
          SMALLINT,
          CHAR(254);

DEFINE v_error_marca SMALLINT;
DEFINE v_marca_error SMALLINT;
DEFINE v_msj_error   CHAR(254);
DEFINE v_id_derechohabiente DECIMAL(9,0);

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_glo_valida_marcaje.trace';
   --TRACE ON;
   
   LET v_error_marca = 0;
   LET v_msj_error   = "MARCA VALIDA";
   LET v_marca_error = 0;
   
   LET v_id_derechohabiente = NULL;

   SELECT id_derechohabiente
     INTO v_id_derechohabiente
     FROM afi_derechohabiente
    WHERE nss = p_nss;
    
   IF( v_id_derechohabiente IS NULL )THEN
      LET v_error_marca = 1;
      LET v_msj_error   = "NO EXISTE DERECHOHABIENTE";
      RETURN v_error_marca,
             v_marca_error,
             v_msj_error;
   END IF
   
   IF EXISTS( SELECT marca
                FROM sfr_marca
               WHERE marca <> 0
                 AND NOT EXISTS( SELECT marca_activa 
                                   FROM sfr_convivencia
                                  WHERE marca_entra = p_marca 
                                    AND marca_activa = marca)) THEN
      LET v_error_marca = 1;
      LET v_msj_error   = "MARCA SIN CUADRAR EN CATÁLOGO";
      RETURN v_error_marca,
             v_marca_error,
             v_msj_error;
   END IF
   
   FOREACH
   SELECT marca_activa
     INTO v_marca_error
     FROM sfr_convivencia, 
          sfr_marca_activa 
    WHERE id_derechohabiente = v_id_derechohabiente
      AND marca        = marca_activa
      AND marca_entra  = p_marca
      AND rch_cod  > 0
    ORDER BY f_inicio DESC
    EXIT FOREACH;
   END FOREACH
    
   IF( v_marca_error <> 0 ) THEN             
      LET v_error_marca = 1;
      LET v_msj_error   = "MARCA RECHAZADA POR CONVIVENCIA";
      RETURN v_error_marca,
             v_marca_error,
             v_msj_error;    
   END IF
             
                
   RETURN v_error_marca,
          v_marca_error,
          v_msj_error;    
END FUNCTION;


