






create procedure "safreviv".fn_valida_43bis(p_folio_liquida      DECIMAL(9,0),
                                 v_f_n_disp           DATE)  --Folio de liquidación del proceso
RETURNING SMALLINT

--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;       --Estatus para inserción
DEFINE v_id_dse_devolucion   DECIMAL(9,0);   --Id devolución
DEFINE v_nss                 CHAR(11);       --Numero de Seguridad Social
DEFINE v_id_derechohabiente  DECIMAL(9,0);   --Id derechohabiente
DEFINE v_num_credito_err     DECIMAL(10,0);  --Numero de credito erroneo
DEFINE v_num_credito_cor     DECIMAL(10,0);  --Numero de credito correcto
DEFINE v_tpo_originacion     SMALLINT;       --Tipo de originación
DEFINE v_monto_aportacion    DECIMAL(12,2);  --Monto aportación
DEFINE v_folio_referencia    DECIMAL(9,0);   --Folio referencia
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
  LET v_nss                = '';
  LET v_id_derechohabiente = 0;
  LET v_num_credito_err    = 0;
  LET v_num_credito_cor    = 0;
  LET v_tpo_originacion    = 0;
  LET v_monto_aportacion   = 0;
  LET v_folio_referencia   = 0;
  LET v_periodo_pago       = '';
  LET v_ban_trabajador     = 0;
  LET v_edo_credito        = 0;
  LET v_dif_num_cred       = 0;
  LET v_edo_procesar       = 0;
  LET v_f_otorga           = '';

  {DROP TABLE dis_dse_num_cred_a;

  CREATE TABLE dis_dse_num_cred_a
               (id_dse_devolucion      DECIMAL(9,0),
                nss                    CHAR(11),
                id_derechohabiente     DECIMAL(9,0),
                num_credito_err        DECIMAL(10,0),
                num_credito_cor        DECIMAL(10,0),
                tpo_originacion        SMALLINT,
                monto_aportacion       DECIMAL(12,2),
                folio_referencia       DECIMAL(9,0),
                periodo_pago           CHAR(6),
                edo_credito            SMALLINT,
                dif_num_cred           SMALLINT)
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;}

  SET PDQPRIORITY HIGH;

  FOREACH
    SELECT dse.id_dse_devolucion,
           dse.id_derechohabiente,
           dse.nss,
           dse.num_credito_cor,
           dse.periodo_pago,
           dse.folio_referencia,
           dse.monto_aportacion
    INTO   v_id_dse_devolucion,
           v_id_derechohabiente,
           v_nss,
           v_num_credito_err,
           v_periodo_pago,
           v_folio_referencia,
           v_monto_aportacion
    FROM   dis_dse_num_cred dse
    WHERE  dse.folio_referencia = p_folio_liquida

    FOREACH
      SELECT cre.num_credito, 
             cre.tpo_originacion, 
             cre.edo_procesar,
             cre.f_otorga
      INTO   v_num_credito_cor, 
             v_tpo_originacion, 
             v_edo_procesar,
             v_f_otorga
      FROM   cre_acreditado cre
      WHERE  cre.id_derechohabiente = v_id_derechohabiente
      ORDER BY f_otorga DESC

      IF v_tpo_originacion = 2  AND
         v_edo_procesar    < 60 THEN
         LET v_tpo_originacion = 0;
         LET v_num_credito_cor = 0;
         LET v_edo_procesar    = 0;
         LET v_f_otorga        = '';
         CONTINUE FOREACH;
      END IF

      LET v_ban_trabajador = 1;
      EXIT FOREACH;
    END FOREACH

    IF v_ban_trabajador = 0 THEN
       LET v_tpo_originacion = 0;
       LET v_num_credito_cor = 0;
       LET v_edo_procesar    = 0;
       LET v_f_otorga        = '';
       LET v_ban_trabajador  = 0;
    ELSE
       LET v_ban_trabajador  = 0;
    END IF

    --Obtiene el estado del credito del derechohabiente si esta liquidado o no
    IF EXISTS (SELECT estado
               FROM   cta_his_credito
               WHERE  id_derechohabiente = v_id_derechohabiente 
               AND    num_credito        = v_num_credito_cor
               AND    f_credito          = v_f_otorga) THEN
       LET v_edo_credito = 2;
    ELSE
       LET v_edo_credito = 0;
    END IF

    IF v_num_credito_err <> v_num_credito_cor THEN
       LET v_dif_num_cred = 1;
    END IF

    INSERT INTO dis_dse_num_cred_a VALUES (v_id_dse_devolucion,
                                           v_nss,
                                           v_id_derechohabiente,
                                           v_num_credito_err,
                                           v_num_credito_cor,
                                           v_tpo_originacion,
                                           v_monto_aportacion,
                                           v_folio_referencia,
                                           v_periodo_pago,
                                           v_edo_credito,
                                           v_dif_num_cred);

    LET v_num_credito_cor = 0;
    LET v_tpo_originacion = 0;
    LET v_edo_procesar    = 0;
    LET v_f_otorga        = '';
    LET v_dif_num_cred    = 0;

  END FOREACH;

  UPDATE STATISTICS FOR TABLE dis_dse_num_cred_a;

  RETURN v_bnd_proceso;

END PROCEDURE;


