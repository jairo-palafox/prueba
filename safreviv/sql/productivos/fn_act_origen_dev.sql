






create procedure "safreviv".fn_act_origen_dev(p_folio_liquida      DECIMAL(9,0))  --Folio de liquidación del proceso
RETURNING SMALLINT

--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;       --Estatus para inserción
DEFINE v_id_dse_devolucion   DECIMAL(9,0);   --Id devolución
DEFINE v_num_credito_cor     DECIMAL(10,0);  --Numero de credito correcto
DEFINE v_tpo_originacion     SMALLINT;       --Tipo Originación
DEFINE v_tpo_transferencia   CHAR(2);        --Tipo Transferencia
DEFINE v_origen_devolucion   CHAR(2);        --Origen devolución
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
  LET v_tpo_originacion    = 0;
  LET v_ban_trabajador     = 0;
  LET v_tpo_transferencia  = 0;
  LET v_origen_devolucion  = 0;

  SET PDQPRIORITY HIGH;

  FOREACH
    SELECT act.id_dse_devolucion,
           act.num_credito_cor,
           act.tpo_originacion
    INTO   v_id_dse_devolucion,
           v_num_credito_cor,
           v_tpo_originacion
    FROM   dis_dse_num_cred act
    WHERE  act.folio_referencia = p_folio_liquida
    AND    act.edo_credito      = 2

    IF v_tpo_originacion = 1 OR
       v_tpo_originacion = 4 THEN
       LET v_tpo_transferencia = '15';
       LET v_origen_devolucion = '03';
    END IF

    IF v_tpo_originacion = 2 THEN
       LET v_tpo_transferencia = '19';
       LET v_origen_devolucion = '02';
    END IF

    UPDATE dse_devolucion 
    --SET    num_credito       = v_num_credito_cor
    SET    tpo_transferencia = v_tpo_transferencia,
           origen_devolucion = v_origen_devolucion
    WHERE  id_dse_devolucion = v_id_dse_devolucion
    AND    folio_referencia  = p_folio_liquida;

    LET v_id_dse_devolucion = 0;
    LET v_num_credito_cor   = 0;
    LET v_tpo_originacion   = 0;
    LET v_tpo_transferencia = 0;
    LET v_origen_devolucion = 0;

  END FOREACH;

  RETURN v_bnd_proceso;

END PROCEDURE;


