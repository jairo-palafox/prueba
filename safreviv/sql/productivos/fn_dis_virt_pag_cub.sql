






create procedure "safreviv".fn_dis_virt_pag_cub()
RETURNING SMALLINT, SMALLINT, CHAR(70),DECIMAL(9,0)

--Última modificación 02042014
--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_precio_fec_hoy      DECIMAL(19,14); --Precio de fondo del dia

DEFINE v_nss                 CHAR(11);       -- NSS del trabajador
DEFINE v_id_derechohabiente  DECIMAL(9,0);   --Id del derechohabiente
DEFINE v_num_credito         DECIMAL(10,0);  --Numero de crédito
DEFINE v_periodo_pago        CHAR(6);        --Periodo de pago
DEFINE v_nrp                 CHAR(11);       --Registro patronal
DEFINE v_apo_avance          DECIMAL(12,2);  --Aportación del Avance de Pago
DEFINE v_amo_avance          DECIMAL(12,2);  --Amortización del Avance de Pago
DEFINE v_apo_pago            DECIMAL(12,2);  --Aportación del pago
DEFINE v_amo_pago            DECIMAL(12,2);  --Amortización del pagoA
DEFINE v_edo_apo             SMALLINT;       --Estado de la compensación de la aportación
DEFINE v_edo_amo             SMALLINT;       --Estado de la compensación de la amortización
DEFINE v_id_dis_det_avance   DECIMAL(9,0);   --Id del avance de pago
DEFINE v_f_pago              DATE;           --Fecha de pago
DEFINE v_estado              SMALLINT;       --Estado del avance de pagos
DEFINE v_f_pago_his          DATE;           --Fecha de pago del historico

DEFINE v_cadena              CHAR(300);
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_bnd_transaccion     SMALLINT;

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info, v_id_derechohabiente;
END EXCEPTION

--Inicialización de variables
LET v_bnd_proceso          = 0; --Estado correcto
LET v_precio_fec_hoy       = 0.00;

LET v_nss                  = "";
LET v_id_derechohabiente   = 0;
LET v_num_credito          = 0;
LET v_periodo_pago         = "";
LET v_nrp                  = "";
LET v_apo_avance           = 0.00;
LET v_amo_avance           = 0.00;
LET v_apo_pago             = 0.00;
LET v_amo_pago             = 0.00;
LET v_edo_apo              = "";
LET v_edo_amo              = "";
LET v_id_dis_det_avance    = 0;
LET v_f_pago               = "";
LET v_estado               = 0;
LET v_f_pago_his           = "";

SET PDQPRIORITY HIGH;

DROP TABLE IF EXISTS dis_virt_pag_cubiertos;
CREATE TABLE dis_virt_pag_cubiertos
              (id_derechohabiente DECIMAL(9,0) NOT NULL,
               nss                CHAR(11),
               num_credito        DECIMAL(10,0),
               periodo_pago       CHAR(6),
               nrp                CHAR(11),
               f_pago             DATE,
               estado             SMALLINT,
               apo_avance         DECIMAL(12,2),
               amo_avance         DECIMAL(12,2),
               apo_pago           DECIMAL(12,2),
               amo_pago           DECIMAL(12,2),
               f_pago_his         DATE,
               edo_apo            SMALLINT,
               edo_amo            SMALLINT)
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

--SET DEBUG FILE TO '/safreviv/dis/sql/fn_dis_virt_pag_cub.TRACE';
--SET DEBUG FILE TO '/home/safreviv/fn_dis_virt_pag_cub.TRACE';
--TRACE ON;

  --Tabla temporar con los Pagos Virtuales
  SELECT ava.id_dis_det_avance_pago,
         ava.periodo_pago,
         ava.id_derechohabiente,
         ava.num_credito,
         ava.f_pago,
         ava.nrp,
         ava.monto_aportacion,
         ava.monto_amortizacion,
         ava.estado,
         afi.nss
  FROM   dis_det_avance_pago ava,
         afi_derechohabiente afi
  WHERE  ava.id_derechohabiente = afi.id_derechohabiente
  --AND    ava.periodo_pago      <= '200505'
  INTO TEMP tmp_dis_virt_pag;

  UPDATE STATISTICS FOR TABLE tmp_dis_virt_pag;

  --Obtiene la información de los Pagos Virtuales
  FOREACH
    SELECT vir.id_dis_det_avance_pago,
           vir.periodo_pago,
           vir.id_derechohabiente,
           vir.num_credito,
           vir.f_pago,
           vir.nrp,
           vir.monto_aportacion,
           vir.monto_amortizacion,
           vir.estado,
           vir.nss
    INTO   v_id_dis_det_avance,
           v_periodo_pago,
           v_id_derechohabiente,
           v_num_credito,
           v_f_pago,
           v_nrp,
           v_apo_avance,
           v_amo_avance,
           v_estado,
           v_nss
    FROM   tmp_dis_virt_pag vir

    --Obtiene la información de la compensación
    SELECT comp.monto_apo_pag,
           comp.monto_amo_pag,
           comp.edo_compensa_apo,
           comp.edo_compensa_amo,
           comp.f_pago
    INTO   v_apo_pago,
           v_amo_pago,
           v_edo_apo,
           v_edo_amo,
           v_f_pago_his
    FROM   dis_compensa_avance comp
    WHERE  comp.id_dis_det_avance_pago = v_id_dis_det_avance
    AND    comp.periodo_pago           = v_periodo_pago
    AND    comp.id_derechohabiente     = v_id_derechohabiente;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       --Si el derechohabiente no tiene COMPENSACIÓN
       INSERT INTO dis_virt_pag_cubiertos VALUES (v_id_derechohabiente,
                                                  v_nss,
                                                  v_num_credito,
                                                  v_periodo_pago,
                                                  v_nrp,
                                                  v_f_pago,
                                                  v_estado,
                                                  v_apo_avance,
                                                  v_amo_avance,
                                                  0.00,
                                                  0.00,
                                                  "",
                                                  "",
                                                  "");
    ELSE
       --Si el derechohabiente no tiene COMPENSACIÓN
       INSERT INTO dis_virt_pag_cubiertos VALUES (v_id_derechohabiente,
                                                  v_nss,
                                                  v_num_credito,
                                                  v_periodo_pago,
                                                  v_nrp,
                                                  v_f_pago,
                                                  v_estado,
                                                  v_apo_avance,
                                                  v_amo_avance,
                                                  v_apo_pago,
                                                  v_amo_pago,
                                                  v_f_pago_his,
                                                  v_edo_apo,
                                                  v_edo_amo);
    END IF
  END FOREACH; --tmp_dis_virt_pag

  UPDATE STATISTICS FOR TABLE dis_virt_pag_cubiertos;

  --TRACE 'Finaliza fn_dis_transaccion con valor '||v_bnd_proceso;
  LET v_char = "  Función finalizo correctamente (Avances)";
  RETURN v_bnd_proceso , 0 , v_char, '';

END PROCEDURE;


