






CREATE FUNCTION "safreviv".fn_reversa_ind_liquidacion(p_folio       DECIMAL(9,0), 
                                           p_proceso_cod SMALLINT,
                                           p_opera_cod   SMALLINT
                                           )
 RETURNING INTEGER, SMALLINT,CHAR(200);

   DEFINE v_sqlQry          CHAR(1024);
   DEFINE v_nom_tabla       VARCHAR(50);
   DEFINE v_bnd_reverso     SMALLINT;
   DEFINE v_i_contador      INTEGER ;
   ---manejo de error
   DEFINE v_sql_error       SMALLINT;
   DEFINE v_isam_error      SMALLINT;
   DEFINE v_msg_error       CHAR(200);
   DEFINE v_si_resultado    SMALLINT;
   -- Captura el error sql
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
	    LET v_si_resultado = v_sql_error ;
      RETURN v_si_resultado,v_isam_error,v_msg_error;
   END EXCEPTION WITH RESUME;
   -- Indica el archivo de errores
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_reversa_ind_liquidacion.trace';
   --TRACE ("Si entra a la función ");
   LET v_sql_error = 0;
   -- Inicializa bandera de ejecucion de reverso
   LET v_bnd_reverso = 0;
   LET v_nom_tabla = NULL;
   -- Recupera el nombre de la tabla de preliquidacion correspondiente
   -- al proceso y operacion
   --se hace el conteo de los registros para validar su existencia
   SELECT COUNT (*)
     INTO v_i_contador
     FROM cta_his_pagos
    WHERE folio_referencia = p_folio
      AND ind_liquidacion = 4 ;
    --si es mayor a 0 significa que si existen regitros   
    IF v_i_contador > 1 THEN 
      --actualiza el registro
      UPDATE cta_his_pagos 
         SET ind_liquidacion = 2 , 
             folio_referencia = 0
       WHERE folio_referencia = p_folio
         AND ind_liquidacion = 4;

    END IF

   --se hace el conteo de los registros para validar su existencia
   SELECT COUNT (*)
     INTO v_i_contador
     FROM cta_his_pagos
    WHERE folio_referencia = p_folio
      AND ind_liquidacion IN (5,6) ;
   --si es mayor a 0 significa que si existen regitros 
   IF v_i_contador > 1 THEN 
      --actualiza el registro
      UPDATE cta_his_pagos 
         SET ind_liquidacion = 1 , 
             folio_referencia = 0
       WHERE folio_referencia = p_folio
         AND ind_liquidacion IN (5,6);

    END IF

 RETURN v_si_resultado,v_isam_error,v_msg_error;
END FUNCTION;


