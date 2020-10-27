






CREATE PROCEDURE "safreviv".sp_dis_fac_apo_subs1(p_folio_factura DECIMAL(9,0)) --Entidad Financiera
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 01092016
--Declaración de variables
DEFINE v_id_derechohabiente        DECIMAL(9,0);  --Id derechohabiente
DEFINE v_concepto                  SMALLINT;      --Concepto
DEFINE v_conteo                    DECIMAL(9,0);  --Conteo de registros
DEFINE v_id_dis_interface_ef       DECIMAL(9,0);  --Id Interface Entidad Financiera
DEFINE v_cve_ent_financiera        SMALLINT;
DEFINE v_cve_ent_financiera_conf   SMALLINT;
DEFINE v_num_ctr_int_ef            CHAR(18);
DEFINE v_num_ctr_int_ef_conf       CHAR(18);
DEFINE v_folio_transaccion         DECIMAL(9,0);
DEFINE v_id_ctr_transaccion        DECIMAL(9,0);
DEFINE v_f_transaccion             DATE;
DEFINE v_edo_ent_financiera        SMALLINT;
DEFINE v_tpo_credito               SMALLINT;
DEFINE v_estado                    SMALLINT; 

--Estado del registro
DEFINE v_bnd_proceso               SMALLINT;
DEFINE v_char                      CHAR(20);

DEFINE v_status                    SMALLINT;
DEFINE sql_err                     INTEGER ;
DEFINE isam_err                    INTEGER ;
DEFINE error_info                  CHAR(70);

DEFINE v_tpo_credito_ocg           CHAR(1);
DEFINE v_resultado                 SMALLINT;
DEFINE v_id_der_credito            DECIMAL(9,0);
DEFINE v_f_otorgamiento            DATE;
DEFINE v_f_liq_credito             DATE;
DEFINE v_id_ocg_formalizacion	   DECIMAL(9,0);

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION

--Inicialización de variables
LET v_id_dis_interface_ef     = 0.00;
LET v_id_derechohabiente      = 0.00;
LET v_concepto                = 0;
LET v_conteo                  = 0.00;

LET v_bnd_proceso             = 0;
LET v_char                    = "";

LET v_status                  = 0;

LET sql_err                   = 0;
LET isam_err                  = 0;
LET error_info                = "";

LET v_cve_ent_financiera      = 0;
LET v_cve_ent_financiera_conf = 0;
LET v_num_ctr_int_ef          = "";
LET v_num_ctr_int_ef_conf     = "";

LET v_folio_transaccion       = 0;
LET v_id_ctr_transaccion      = 0;
LET v_f_transaccion           = "";
LET v_edo_ent_financiera      = 0;
LET v_tpo_credito             = 0;
LET v_estado                  = 0;

LET v_tpo_credito_ocg         = "";
LET v_resultado               = 0;
LET v_id_der_credito          = 0;
LET v_f_otorgamiento          = "";
LET v_f_liq_credito           = "";
LET v_id_ocg_formalizacion	  = 0;

  FOREACH
    --Busca los id_derechohabiente de la tabla principal
    SELECT a.id_dis_interface_ef, a.id_derechohabiente, a.concepto, 
           a.cve_ent_financiera, a.num_ctr_int_ef, a.folio_transaccion, 
           a.id_ctr_transaccion, a.f_transaccion, a.tpo_credito
    INTO   v_id_dis_interface_ef, v_id_derechohabiente, v_concepto,
           v_cve_ent_financiera_conf, v_num_ctr_int_ef_conf, v_folio_transaccion,
           v_id_ctr_transaccion, v_f_transaccion, v_tpo_credito
    FROM   tmp_dis_fac_aps_tns a

    LET v_estado = 0;
     
    --1)Busca si esta registrado el derechohabiente, Tipos de Crédito Apoyo INFONAVIT
    --1)Busca si esta registrado el derechohabiente, Tipos de Crédito COFINAVIT
    --########## Se agrega función para identificar el crédito ###########
    --Valor resultado = 0:  Sin Crédito
    --Valor resultado = 1:  Crédito Vigente
    --Valor resultado = 2:  Crédito Liquidado
    --Valor tpo_credito = A o C: Apoyo INFONAVIT
    --Valor tpo_credito = 7 u 8: COFINANCIADOS
    LET v_tpo_credito_ocg       = "";
    LET v_resultado             = 0;
    LET v_id_der_credito        = 0;
    LET v_f_otorgamiento        = "";
    LET v_f_liq_credito         = "";
    LET v_cve_ent_financiera    = 0;
    LET v_num_ctr_int_ef        = '';
    LET v_id_ocg_formalizacion  = 0;

    EXECUTE FUNCTION fn_credito_43bis(v_id_derechohabiente)
                INTO v_resultado,
                     v_id_der_credito,
                     v_tpo_credito_ocg,
                     v_f_otorgamiento,
                     v_f_liq_credito,
                     v_cve_ent_financiera,
                     v_num_ctr_int_ef,
                     v_id_ocg_formalizacion;

    --Verificar si existe en BD ACREDITADOS
    IF v_resultado = 0 THEN
       --Si el derechohabiente no esta registrado se modifica el estatus de la tabla de aportaciones subsecuentes.
       LET v_estado = 31;
       
       UPDATE dis_ctr_aps_tns 
       SET    estado             = v_estado --Por facturar no acréditado
       WHERE  folio_transaccion  = v_folio_transaccion
       AND    id_ctr_transaccion = v_id_ctr_transaccion;       
    ELSE
       LET v_edo_ent_financiera = 0;
       LET v_num_ctr_int_ef     = "";
       
       IF v_cve_ent_financiera_conf <> v_cve_ent_financiera THEN
          SELECT cat.cve_bloqueo
          INTO   v_edo_ent_financiera
          FROM   cat_cta_cnt_ocg cat
          WHERE  cat.cve_ent_financiera = v_cve_ent_financiera
          AND    cat.tpo_credito        = v_tpo_credito;
          IF v_edo_ent_financiera = 1 THEN -- Entidad Financiera Bloqueada
             LET v_estado = 40; -- 40  - Entidad Financiera Bloqueada

             UPDATE dis_ctr_aps_tns
             SET    cve_ent_financiera = v_cve_ent_financiera,
                    num_ctr_inf_ef     = v_num_ctr_int_ef,
                    estado             = v_estado
             WHERE  folio_transaccion  = v_folio_transaccion
             AND    id_ctr_transaccion = v_id_ctr_transaccion;
          ELSE
             UPDATE dis_ctr_aps_tns
             SET    cve_ent_financiera = v_cve_ent_financiera,
                    num_ctr_inf_ef     = v_num_ctr_int_ef
             WHERE  folio_transaccion  = v_folio_transaccion
             AND    id_ctr_transaccion = v_id_ctr_transaccion;
          END IF

          INSERT INTO dis_ctr_ent_financiera_aps VALUES (v_folio_transaccion,
                                                         v_id_ctr_transaccion,
                                                         v_f_transaccion,
                                                         v_id_derechohabiente,
                                                         v_cve_ent_financiera_conf,
                                                         v_num_ctr_int_ef_conf,
                                                         v_cve_ent_financiera,
                                                         v_num_ctr_int_ef,
                                                         TODAY);
       END IF
       
       LET v_cve_ent_financiera_conf = 0;
       LET v_num_ctr_int_ef_conf     = "";
    END IF
  END FOREACH

  LET v_char = "Terminada transacción 1 correctamente";
  RETURN v_bnd_proceso , 0 , v_char;
  
END PROCEDURE;


