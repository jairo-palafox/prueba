






create procedure "safreviv".fn_folio_referencia(p_folio_liquida      DECIMAL(9,0))
RETURNING SMALLINT

--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;       --Estatus para inserción
DEFINE v_id_dse_devolucion   DECIMAL(9,0);   --Id devolución
DEFINE v_id_referencia       DECIMAL(9,0);   --Id devolución
DEFINE v_nss                 CHAR(11);       --Numero de Seguridad Social
DEFINE v_id_derechohabiente  DECIMAL(9,0);   --Id derechohabiente
DEFINE v_num_credito_err     DECIMAL(10,0);  --Numero de credito erroneo
DEFINE v_num_credito_cor     DECIMAL(10,0);  --Numero de credito correcto
DEFINE v_tpo_originacion     SMALLINT;       --Tipo de originación
DEFINE v_monto_aportacion    DECIMAL(12,2);  --Monto aportación
DEFINE v_folio_referencia    DECIMAL(9,0);   --Folio referencia
DEFINE v_folio_liquida       DECIMAL(9,0);   --Folio referencia
DEFINE v_periodo_pago        CHAR(6);        --Periodo pago
DEFINE v_ban_trabajador      SMALLINT;       --Bandera tipo trabajador
DEFINE v_edo_credito         SMALLINT;       --Estado del crédito
DEFINE v_dif_num_cred        SMALLINT;       --Diferencia numero de credito
DEFINE v_edo_procesar        SMALLINT;       --Estado crédito PROCESAR
DEFINE v_f_otorga            DATE;           --Fecha otorgamiento crédito

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
  LET v_id_referencia      = 0;
  LET v_nss                = '';
  LET v_id_derechohabiente = 0;
  LET v_num_credito_err    = 0;
  LET v_num_credito_cor    = 0;
  LET v_tpo_originacion    = 0;
  LET v_monto_aportacion   = 0;
  LET v_folio_referencia   = 0;
  LET v_folio_liquida      = 0;
  LET v_periodo_pago       = '';
  LET v_ban_trabajador     = 0;
  LET v_edo_credito        = 0;
  LET v_dif_num_cred       = 0;
  LET v_edo_procesar       = 0;
  LET v_f_otorga           = '';

  SET PDQPRIORITY HIGH;

  FOREACH
    SELECT cta.id_referencia,
           cta.folio_liquida,
           dse.folio_referencia
    INTO   v_id_referencia,
           v_folio_liquida,
           v_folio_referencia
    FROM   dse_devolucion dse, 
           cta_movimiento cta
    WHERE  dse.id_dse_devolucion = cta.id_referencia 
    AND    cta.folio_liquida  IN (
                                     9990,
                                     10327,
                                     10368,
                                     10379,
                                     10380,
                                     10381,
                                     10559,
                                     10602,
                                     10712,
                                     10764,
                                     10818,
                                     10990,
                                     11075,
                                     11139,
                                     11233,
                                     11281,
                                     11333,
                                     11401,
                                     11468,
                                     11511,
                                     11562
                                    )
    AND    cta.movimiento       IN (602,782)
    AND    dse.folio_referencia IN (11734,11735)
    AND    dse.modulo_cod        = 'dis'

    UPDATE dse_devolucion
    SET    folio_referencia  = v_folio_liquida,
           folio             = v_folio_referencia
    WHERE  id_dse_devolucion = v_id_referencia
    AND    folio_referencia IN (11734,11735)
    AND    modulo_cod        = 'dis' ;

    LET v_id_referencia    = 0;
    LET v_folio_referencia = 0;

  END FOREACH;

  RETURN v_bnd_proceso;

END PROCEDURE;


