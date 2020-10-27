






CREATE PROCEDURE "safreviv".sp_dis_casos_excepcion(p_folio    DECIMAL(9,0), 
                                        p_edo_rech SMALLINT,
                                        p_usuario  VARCHAR(30))

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 30062014
--Declaración de variables
DEFINE v_id_ref_seq             DECIMAL(9,0); --Secuencia
DEFINE v_nss 		        CHAR(11);
DEFINE v_num_credito            DECIMAL(10,0);
DEFINE v_periodo_pago           CHAR(6);
DEFINE v_f_pago                 DATE;
DEFINE v_ent_recaudadora        CHAR(3);
DEFINE v_nrp                    CHAR(11);
DEFINE v_aportacion             DECIMAL(8,2);
DEFINE v_amortizacion           DECIMAL(8,2);
DEFINE v_folio_sua              DECIMAL(6,0);

DEFINE v_id_derechohabiente     DECIMAL(9,0);
DEFINE v_estado 	        SMALLINT;
DEFINE v_desc_resultado         CHAR(40);

DEFINE v_bnd_transaccion        SMALLINT;
DEFINE v_bnd_proceso            SMALLINT; --Estatus del proceso
DEFINE v_status                 SMALLINT;
DEFINE sql_err                  INTEGER ;
DEFINE isam_err                 INTEGER ;
DEFINE error_info               CHAR(70);
DEFINE v_char                   CHAR(20);

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;  
END EXCEPTION

   --#Inicialización de variables
   LET v_id_ref_seq             = 0;
   LET v_nss 		        = "";
   LET v_num_credito            = 0;
   LET v_periodo_pago           = "";
   LET v_f_pago                 = "";
   LET v_ent_recaudadora        = "";
   LET v_nrp                    = "";
   LET v_aportacion             = 0.00;
   LET v_amortizacion           = 0.00;
   LET v_folio_sua              = 0;

   LET v_id_derechohabiente     = 0;
   LET v_estado                 = 0;
   LET v_desc_resultado         = "";

   LET v_bnd_proceso            = 0; --Estado correcto
   LET v_bnd_transaccion        = 0;

--SET DEBUG FILE TO '/ds/safreviv/dis/sql/sp_dis_casos_excepcion.TRACE';
--SET DEBUG FILE TO '/home/safreviv/sp_dis_casos_excepcion.TRACE';
-- SET DEBUG FILE TO 'sp_dis_casos_excepcion.TRACE';
-- TRACE ON;

   SET PDQPRIORITY HIGH;

   SET INDEXES FOR dis_caso_excepcion DISABLED; 

   FOREACH
     SELECT cas.nss,
            cas.num_credito,
            cas.periodo_pago,
            cas.f_pago,
            cas.ent_recaudadora,
            cas.nrp,
            cas.aportacion   / 100,
            cas.amortizacion / 100,
            cas.folio_sua
     INTO   v_nss,
            v_num_credito,
            v_periodo_pago,
            v_f_pago,
            v_ent_recaudadora,
            v_nrp,
            v_aportacion,
            v_amortizacion,
            v_folio_sua
     FROM   safre_tmp:tmp_casos_exc1 cas

     LET v_estado = 0; --registro integrado
  
     --Validación de Negocio NSS 
     IF v_nss IS NULL THEN
        LET v_estado         = 10; --nss nulo
        --LET v_desc_resultado = 'NSS NULO';
     ELSE
        IF LENGTH(v_nss) <> 11 THEN
           LET v_estado = 20; --nss diferente a 11 posiciones
        ELSE
           --#Obtenemos id_derechohabiente
           SELECT afi.id_derechohabiente
           INTO   v_id_derechohabiente
           FROM   afi_derechohabiente afi
           WHERE  afi.nss = v_nss;
           --#Asigna id_derechohabiente si no se encuentra en tabla
           IF v_id_derechohabiente IS NULL THEN
              LET v_id_derechohabiente = "999999999";
      
              --rechazo por no existir en el maestro de derechohabientes
              LET v_estado = 30; --nss no existe en la bd de afiliados
           END IF
        END IF
     END IF

     --Validación de Negocio Numero de Crédito
     IF v_estado = 0 THEN
        IF v_num_credito IS NULL THEN
           LET v_estado = 40; --numero de crédito nulo
        END IF
     END IF

     --Validación de Negocio Período de Pago
     IF v_estado = 0 THEN
        IF v_periodo_pago      IS NULL OR
           LENGTH(v_periodo_pago) <> 6 THEN
           LET v_estado = 50; --periodo de pago nulo
        END IF
     END IF

     --Validación de Negocio Fecha de Pago
     IF v_estado = 0 THEN
        IF v_f_pago IS NULL THEN
           LET v_estado = 60; --fecha de pago nula
        END IF
     END IF

     --Validación de Negocio Entidad Recaudadora
     IF v_estado = 0 THEN
        IF v_ent_recaudadora      IS NULL OR 
           LENGTH(v_ent_recaudadora) <> 3 THEN
           LET v_estado = 70; --entidad recaudadora nula
        END IF
     END IF

     --Validación de Negocio NRP
     IF v_estado = 0 THEN
        IF v_nrp         IS NULL OR 
           LENGTH(v_nrp) <> 11 THEN
           LET v_estado = 80; --nrp nulo
        END IF
     END IF

     --Validación de Negocio Monto Aportación
     IF v_estado = 0 THEN
        IF v_aportacion < 0.00 THEN
           LET v_estado = 90; --monto aportación menor a 0
        END IF
     END IF
			
     --Validación de Negocio Monto Amortización
     IF v_estado = 0 THEN
        IF v_amortizacion < 0.00 THEN
           LET v_estado = 100; --monto amortización menor a 0
        END IF
     END IF

     --Validación de Negocio Valida Montos
     IF v_estado = 0 THEN
         IF v_aportacion <= 0.00 AND v_amortizacion <= 0.00 THEN
            LET v_estado = 110; --suma de montos menor o igual a 0
         END IF
     END IF

     --Validación de Negocio Folio SUA
     IF v_estado = 0 THEN
        IF v_folio_sua IS NULL THEN
           LET v_estado = 120; --folio sua nulo
        END IF
     END IF

     LET v_id_ref_seq = seq_dis_interface_ce.NEXTVAL;

     --Integra en la tabla los casos de excepción a dispersar
     INSERT INTO dis_caso_excepcion VALUES (v_id_ref_seq,
                                            v_id_derechohabiente,
                                            p_folio,
                                            v_nss,
                                            v_num_credito,
                                            v_periodo_pago,
                                            v_f_pago,
                                            v_ent_recaudadora,
                                            v_nrp,
                                            v_aportacion,
                                            v_amortizacion,
                                            v_folio_sua,
	                                    v_estado,
                                            TODAY,
                                            p_usuario);
   END FOREACH;

   SET INDEXES FOR dis_caso_excepcion ENABLED;

   UPDATE STATISTICS FOR TABLE dis_caso_excepcion;

-- TRACE 'Finaliza sp_dis_caso_excepcion con valor '||v_bnd_proceso;   
   LET v_char = "Terminado Cas_excep_SPL";
   RETURN v_bnd_proceso , 0 , v_char;
  
END PROCEDURE;


