






CREATE PROCEDURE "safreviv".fn_dis_ext_ava_pag(p_periodo_pago CHAR(6))

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 20072015
--Declaración de variables

DEFINE v_periodo_pago        CHAR(6);        --Periodo de Pago
DEFINE v_nss                 CHAR(11);       --Número de Seguridad Social
DEFINE v_num_credito         DECIMAL(10,0);  --Número de Crédito
DEFINE v_f_pago              DATE;           --Fecha de pago
DEFINE v_nrp                 CHAR(11);       --Número de registro patronal
DEFINE v_monto_aportacion    DECIMAL(12,2);  --Importe aportaciones patronales 
DEFINE v_monto_amortizacion  DECIMAL(12,6);  --Importe amortizaciones de credito
DEFINE v_estado              SMALLINT;       --Estado de avance
DEFINE v_desc_edo_avance     CHAR(50);       --Descripción del estado de avance
DEFINE v_folio               DECIMAL (9,0);  --Número de Folio
DEFINE v_f_actualiza         DATE;           --Fecha proceso del folio   
DEFINE v_tipo_interface 		 CHAR(4);        --Tipo de interface

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


--SET DEBUG FILE TO '/ds/safreviv_int/dis/respaldo/PRODINF-860/fn_dis_ext_ava_pag.TRACE';
--TRACE ON;

--Inicialización de variables
LET v_periodo_pago        ="";
LET v_nss                 ="";
LET v_num_credito         =0.00;
LET v_nrp                 ="";
LET v_monto_aportacion    =0.00;
LET v_monto_amortizacion  =0.00;
LET v_estado              =0;
LET v_desc_edo_avance     ="";
LET v_folio               =0.00;
LET v_tipo_interface 		  ="";

LET v_bnd_proceso             = 0;
LET v_char                    = "";

LET v_status                  = 0;

LET sql_err                   = 0;
LET isam_err                  = 0;
LET error_info                = "";


   DROP TABLE IF EXISTS tmp_dis_ava_pag;
   CREATE TABLE tmp_dis_ava_pag (periodo_pago        CHAR(6),
                                 nss                 CHAR(11),     
                                 num_credito         DECIMAL(10,0),
                                 f_pago              DATE,         
                                 nrp                 CHAR(11),
                                 monto_aportacion    DECIMAL(12,2),
                                 monto_amortizacion  DECIMAL(12,6),
                                 estado              SMALLINT,
                                 desc_edo_avance     CHAR(50),     
                                 folio               DECIMAL (9,0),
                                 f_actualiza         DATE,
                                 f_interface         DATE,
                                 tipo_interface 		 CHAR(4)
                                 )
   FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

   SET PDQPRIORITY HIGH;

   FOREACH
      SELECT ap.periodo_pago, 
             ad.nss,
             ap.num_credito,  
             ap.f_pago, 
             ap.nrp,
             ap.monto_aportacion, 
             ap.monto_amortizacion, 
             ap.estado,
             ce.desc_edo_avance,
             ap.folio,
             gf.f_actualiza,             
             'APPV'
        INTO v_periodo_pago,       
             v_nss,                
             v_num_credito,        
             v_f_pago,             
             v_nrp,                
             v_monto_aportacion,   
             v_monto_amortizacion,
             v_estado, 
             v_desc_edo_avance,    
             v_folio,              
             v_f_actualiza,        
             v_tipo_interface 		       
        FROM dis_det_avance_pago ap, 
             afi_derechohabiente ad,
             cat_edo_avance_pago ce, 
             glo_folio gf              
       WHERE ap.id_derechohabiente = ad.id_derechohabiente
         AND ap.estado = ce.cod_edo_avance
         AND ap.estado IN (30, 50, 70, 80, 85)
         AND gf.folio = ap.folio
         AND ap.periodo_pago = p_periodo_pago
        

      INSERT INTO tmp_dis_ava_pag VALUES (v_periodo_pago,
                                          v_nss,
                                          v_num_credito,
                                          v_f_pago,
                                          v_nrp,
                                          v_monto_aportacion,
                                          v_monto_amortizacion,
                                          v_estado,
                                          v_desc_edo_avance,
                                          v_folio,
                                          v_f_actualiza,
                                          TODAY,
                                          v_tipo_interface
                                          );

   END FOREACH;
   
   --TRACE 'Finaliza fn_dis_ext_ava_pag con valor '||v_bnd_proceso;
   
   LET v_char = "Terminado Extractor Aportaciones Subsecuentes por periodo de pago";
   RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


