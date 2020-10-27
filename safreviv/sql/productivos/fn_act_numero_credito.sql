






create procedure "safreviv".fn_act_numero_credito(p_folio_liquida      DECIMAL(9,0))  --Folio de liquidación del proceso
RETURNING SMALLINT

--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;       --Estatus para inserción
DEFINE v_id_dse_devolucion   DECIMAL(9,0);   --Id devolución
DEFINE v_num_credito_cor     DECIMAL(10,0);  --Numero de credito correcto
DEFINE v_ban_trabajador      SMALLINT;       --Bandera tipo trabajador

DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status;
END EXCEPTION

  --Inicializacón de variables
  LET v_bnd_proceso        = 1; --Estado correcto
  LET v_id_dse_devolucion  = 0;
  LET v_num_credito_cor    = 0;
  LET v_ban_trabajador     = 0;

  SET PDQPRIORITY HIGH;

  FOREACH
    SELECT act.id_dse_devolucion,
           act.num_credito_cor
    INTO   v_id_dse_devolucion,
           v_num_credito_cor 
    FROM   dis_dse_num_cred act
    WHERE  act.folio_referencia = p_folio_liquida
    AND    act.dif_num_cred     = 1
    AND    act.edo_credito      = 2

    UPDATE dse_devolucion 
    SET    num_credito       = v_num_credito_cor
    WHERE  id_dse_devolucion = v_id_dse_devolucion
    AND    folio_referencia  = p_folio_liquida;

    LET v_num_credito_cor   = 0;
    LET v_id_dse_devolucion = 0;

  END FOREACH;

  RETURN v_bnd_proceso;

END PROCEDURE;


