






CREATE FUNCTION "safreviv".fn_dac_integra_mtc(p_usuario_cod   CHAR(20),
                                   p_pid           DECIMAL(9,0),
                                   p_folio_integra DECIMAL(10),
                                   p_proceso_cod   SMALLINT)

RETURNING INTEGER,  INTEGER, CHAR(200),INTEGER, INTEGER, INTEGER

--Se definen las variables                                       
DEFINE v_tipo_registro_01     CHAR(2);                                       
DEFINE v_fecha_archivo        CHAR(8);                                       
DEFINE v_tot_solicitudes      INTEGER;                                        
DEFINE v_tot_aceptadas        INTEGER;                                        
DEFINE v_tot_rechazadas       INTEGER;                                       

DEFINE v_tipo_registro_02     CHAR(2);                                        
DEFINE v_id_dac_solicitud     DECIMAL(9,0);
DEFINE v_folio_integracion    DECIMAL(9,0);
DEFINE v_num_credito          DECIMAL(10,0);
DEFINE v_nss                  CHAR(11); 
DEFINE v_periodo_pago         CHAR(4);
DEFINE v_imp_amortizacion     DECIMAL(16,6); 
DEFINE v_folio_sua            CHAR(6); --DECIMAL(6,0);
DEFINE v_nrp                  CHAR(11);
DEFINE v_id_sdd               CHAR(6); 
DEFINE v_resul_opera          SMALLINT;
DEFINE v_diagnostico          SMALLINT;
DEFINE v_folio_ajuste         DECIMAL(9,0);
DEFINE v_folio_liquidacion    DECIMAL(9,0);
DEFINE v_id_derechohabiente   DECIMAL(9,0);
DEFINE v_fecha_correcta       DATE;
DEFINE v_dis_nss              CHAR(11);
DEFINE v_dis_folio_sua        DECIMAL(9,0);
DEFINE v_dis_periodo_pago     CHAR(4);
DEFINE v_dis_nrp              CHAR(11);
DEFINE v_dis_numero_credito   CHAR(10);
DEFINE v_dis_imp_amortizacion DECIMAL(16,6);    
DEFINE v_subcuenta41          SMALLINT;      
DEFINE v_subcuenta            SMALLINT;      
DEFINE v_fondo_inversion      SMALLINT;     
DEFINE v_acciones             DECIMAL(16,6);
DEFINE v_pesos                DECIMAL(16,6);
DEFINE v_cta_marcada          SMALLINT; 
--Rechazos
DEFINE v_id_rechazo           DECIMAL(9,0);
DEFINE v_folio                DECIMAL(9,0);
DEFINE v_tipo_registro        SMALLINT    ;
DEFINE v_campo_valor          CHAR(50)    ;
-- Control de Excepciones
DEFINE sql_err                INTEGER;
DEFINE v_isam_err             INTEGER;
DEFINE isam_err               INTEGER;
DEFINE err_txt                CHAR(200);
DEFINE v_resultado            SMALLINT;
DEFINE v_periodo_pago_bim     CHAR(2);
DEFINE v_periodo_pago_dis     CHAR(4);

ON EXCEPTION SET sql_err, isam_err, err_txt
   LET v_resultado = sql_err;
   LET err_txt     = "Error";
   LET v_isam_err  = isam_err;
   LET v_tot_solicitudes = 0; 
   LET v_tot_aceptadas   = 0;   
   LET v_tot_rechazadas  = 0;   
   
   RETURN v_resultado, 
          v_isam_err,
          err_txt,
          v_tot_solicitudes, 
          v_tot_aceptadas,
          v_tot_rechazadas;
END EXCEPTION

--Se inician las variables 
LET v_tipo_registro_01     = 0;
LET v_fecha_archivo        = 0;
LET v_tot_solicitudes      = 0;
LET v_tot_aceptadas        = 0;
LET v_tot_rechazadas       = 0;
LET v_tipo_registro_02     = "";
LET v_num_credito          = "";
LET v_nss                  = "";
LET v_periodo_pago         = "";
LET v_imp_amortizacion     = 0; 
LET v_folio_sua            = 0;
LET v_nrp                  = "";
LET v_id_sdd               = "";
LET v_resul_opera          = 0;
LET v_diagnostico          = 0;
LET v_id_derechohabiente   = 0;
LET v_fecha_correcta       = "";
LET v_dis_nss              = "";
LET v_dis_folio_sua        = 0;
LET v_dis_periodo_pago     = "";
LET v_dis_nrp              = "";
LET v_dis_numero_credito   = "";
LET v_dis_imp_amortizacion = 0;
LET v_subcuenta41          = 41;
LET v_subcuenta            = 0;     
LET v_fondo_inversion      = 0;
LET v_acciones             = 0;
LET v_pesos                = 0;
LET v_id_dac_solicitud     = 0;
LET v_folio_ajuste         = 0;
LET v_folio_liquidacion    = NULL;
LET v_folio_integracion    = 0;
LET v_cta_marcada          = 0;
--Rechazos
LET v_id_rechazo           =0;
LET v_folio                =0;
LET v_tipo_registro        =0;
LET v_resul_opera          =1;
LET v_diagnostico          =1;
LET v_campo_valor          ="";       
LET v_isam_err             =0;    
LET v_resultado            =0;
LET err_txt                = "Ok";
LET v_periodo_pago_bim     ="";
LET v_periodo_pago_dis     ="";

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace_fn_dac_integra_mtc.txt";
   --TRACE ON; 
-------------------------
   --Valida el encabezado
-------------------------
   SELECT *
   INTO   v_tipo_registro_01  ,
          v_fecha_archivo
   FROM   safre_tmp:tmp_cza_dev_amortizacion_mtc;
   
   IF (v_tipo_registro_01 <> "01" OR v_tipo_registro_01 IS NULL ) THEN 
      LET v_resul_opera = 2;
      LET v_diagnostico = 11;

      EXECUTE PROCEDURE sp_dac_inserta_rechazo(p_folio_integra,
                                               "01"           ,
                                               v_fecha_archivo,
                                               v_resul_opera  ,
                                               v_diagnostico  ,
                                               "ERROR EN EL ENCABEZADO"
                                               );
   END IF    

   --EXECUTE PROCEDURE sp_cambia_formato_fecha(v_fecha_archivo)
   --INTO v_fecha_correcta;
   --
   --IF ( v_fecha_correcta > TODAY OR v_fecha_correcta IS NULL ) THEN 
   --   LET v_resul_opera = 2;
   --   LET v_diagnostico = 1;
   --   
   --   EXECUTE PROCEDURE sp_dac_inserta_rechazo(p_folio_integra         ,
   --                                            "01"                    ,
   --                                            v_id_dac_solicitud      ,
   --                                            v_resul_opera           ,
   --                                            v_diagnostico           ,
   --                                            "ERROR EN EL ENCABEZADO"
   --                                            );
   --END IF
-------------------------
   --Valida detalles
-------------------------
   --trace "Al recuperar datos detalle tmp_det_dev_amortizacion_mtc";
   LET err_txt = "Al recuperar datos detalle tmp_det_recauda_hipoteca";  
   FOREACH
      SELECT tipo_registro   ,
             num_credito     ,
             nss             ,
             periodo_pago    ,
             imp_amortizacion / 100,
             folio_sua       ,
             nrp             ,
             id_sdd          
      INTO   v_tipo_registro_02,
             v_num_credito     ,
             v_nss             ,
             v_periodo_pago    ,
             v_imp_amortizacion,
             v_folio_sua       ,
             v_nrp             ,
             v_id_sdd
      FROM   safre_tmp:tmp_det_dev_amortizacion_mtc

      SELECT seq_dac_det_solicitud.NEXTVAL
      INTO   v_id_dac_solicitud                
	    FROM   SYSTABLES                        
	    WHERE tabname = "dac_det_solicitud";
                        
      --Se inician valores como aceptados
      LET v_resul_opera = 1;
      LET v_diagnostico = 1;

      --Valida que el tipo de registro sea  2 'Detalles'
      LET err_txt = "Valida que el tipo de registro";
      IF (v_tipo_registro_02 <> "02" OR v_tipo_registro_02 IS NULL ) THEN 
         LET v_resul_opera = 2;
         LET v_diagnostico = 12;

         EXECUTE PROCEDURE sp_dac_inserta_rechazo(p_folio_integra   ,
                                                  "02"              ,
                                                  v_id_dac_solicitud,
                                                  v_resul_opera     ,
                                                  v_diagnostico     ,
                                                  "ERROR EN EL DETALLE"
                                                  );
         
      END IF 
      
      --Valida que el NSS no sea nulo ";
      LET err_txt = "Valida el nss";     
      IF (v_nss  IS NULL) THEN        
         LET v_resul_opera = 2;
         LET v_diagnostico = 13;
         
         EXECUTE PROCEDURE sp_dac_inserta_rechazo(p_folio_integra   ,
                                                  "02"              ,
                                                  v_id_dac_solicitud,
                                                  v_resul_opera     ,
                                                  v_diagnostico     ,
                                                  "NO EXISTE NSS EN SACI"
                                                  );
      END IF
      
      --Valida que NSS exista dentro de la BD de SAFRE-SACI
      SELECT UNIQUE (id_derechohabiente)
      INTO  v_id_derechohabiente
      FROM  afi_derechohabiente 
      WHERE nss = v_nss;
      
      LET err_txt = "Id_derechohabiente, no existe";
      IF(v_id_derechohabiente IS NULL)THEN
         LET v_resul_opera = 2;
         LET v_diagnostico = 13;

         EXECUTE PROCEDURE sp_dac_inserta_rechazo(p_folio_integra   ,
                                                  "02"              ,
                                                  v_id_dac_solicitud,
                                                  v_resul_opera     ,
                                                  v_diagnostico     ,
                                                  "NO EXISTE NSS EN SACI"
                                                  );         
      END IF

      --Valida que este marcada la cuenta en “ACLARACIÓN-AMORTIZACIÓN NO DISPERSADA” (450)
      SELECT marca
      INTO   v_cta_marcada
      FROM   sfr_marca_activa
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    marca = 450;

      IF v_cta_marcada IS NULL THEN 
         LET v_resul_opera = 2;
         LET v_diagnostico = 14;
         
         EXECUTE PROCEDURE sp_dac_inserta_rechazo(p_folio_integra   ,
                                                  "02"              ,
                                                  v_id_dac_solicitud,
                                                  v_resul_opera     ,
                                                  v_diagnostico     ,
                                                  "CUENTA SIN MARCAR"
                                                  );         
      END IF
      
      --Se agrega conversión de periodo de pago recibe mensual, convertir a bimestral (AAMM-AABB) #AG-29042014
      IF v_periodo_pago[3,4] IS NULL OR v_periodo_pago[3,4] = "00" OR v_periodo_pago[3,4] = "  " THEN 
         LET v_resul_opera = 2;
         LET v_diagnostico = 13;
      
         EXECUTE PROCEDURE sp_dac_inserta_rechazo(p_folio_integra   ,
                                                  "02"              ,
                                                  v_id_dac_solicitud,
                                                  v_resul_opera     ,
                                                  v_diagnostico     ,
                                                  "PERIODO DE PAGO NO VÁLIDO"
                                                  );  
      ELSE
         IF v_periodo_pago[3,4] = "01" OR v_periodo_pago[3,4] = "02" THEN
            LET v_periodo_pago_bim = "01";
         ELSE 
            IF v_periodo_pago[3,4] = "03" OR v_periodo_pago[3,4] = "04" THEN
               LET v_periodo_pago_bim = "02";
            ELSE
               IF v_periodo_pago[3,4] = "05" OR v_periodo_pago[3,4] = "06" THEN
                  LET v_periodo_pago_bim = "03";
               ELSE
                  IF v_periodo_pago[3,4] = "07" OR v_periodo_pago[3,4] = "08" THEN
                     LET v_periodo_pago_bim = "04";
                  ELSE
                     IF v_periodo_pago[3,4] = "09" OR v_periodo_pago[3,4] = "10" THEN
                        LET v_periodo_pago_bim = "05";
                     ELSE
                        IF v_periodo_pago[3,4] = "11" OR v_periodo_pago[3,4] = "12" THEN
                           LET v_periodo_pago_bim = "06";
                        END IF --PP06
                     END IF --PP05
                  END IF --PP04
               END IF --PP03
            END IF --PP02
         END IF --PP01    
      END IF --PERIODO NULO

      LET v_periodo_pago_dis = v_periodo_pago[1,2] || v_periodo_pago_bim;
                
      --Valida que exista el registro dentro de la tabla de dispersión para devolver
      SELECT nss,
             folio_ajuste
      INTO   v_dis_nss,
             v_folio_ajuste
      FROM   dis_ajuste_op_mtc
      WHERE  nss          = v_nss
      AND    folio_sua    = v_folio_sua
      AND    periodo_pago[3,6] = v_periodo_pago_dis
      AND    nrp          = v_nrp
      AND    num_crd_ifv  = v_num_credito;

      IF ( v_dis_nss IS NULL )THEN
         LET v_resul_opera = 2;
         LET v_diagnostico = 15;

         EXECUTE PROCEDURE sp_dac_inserta_rechazo(p_folio_integra   ,
                                                  "02"              ,
                                                  v_id_dac_solicitud,
                                                  v_resul_opera     ,
                                                  v_diagnostico     ,
                                                  "NO EXISTE REGISTRO EN DISPERSIÓN"
                                                  );
      END IF

      --Valida que el NSS tenga el saldo solicitado en la subcuenta de amortización (41)
      FOREACH 
         EXECUTE FUNCTION fn_saldo_actual(v_nss,41,TODAY)
         INTO v_subcuenta,
              v_fondo_inversion,
              v_acciones,
              v_pesos
      END FOREACH;

      IF (v_pesos < 0 AND v_acciones < 0) THEN 
         LET v_resul_opera = 2;
         LET v_diagnostico = 16;
         
         EXECUTE PROCEDURE sp_dac_inserta_rechazo(p_folio_integra   ,
                                                  "02"              ,
                                                  v_id_dac_solicitud,
                                                  v_resul_opera     ,
                                                  v_diagnostico     ,
                                                  "CUENTA SIN SALDO"
                                                  );
      END IF
      
      {
      IF (v_pesos < v_imp_amortizacion) THEN 
         LET v_resul_opera = 2;
         LET v_diagnostico = 17;
         
         EXECUTE PROCEDURE sp_dac_inserta_rechazo(p_folio_integra   ,
                                                  "02"              ,
                                                  v_id_dac_solicitud,
                                                  v_resul_opera     ,
                                                  v_diagnostico     ,
                                                  "MONTOS DIFERENTES"
                                                  );
      END IF
      }

      INSERT INTO dac_det_solicitud
                 (
                  id_dac_solicitud  ,
                  id_derechohabiente,
                  folio_integracion ,
                  num_credito       ,
                  nss               ,
                  periodo_pago      ,
                  imp_amortizacion  ,
                  folio_sua         ,
                  nrp               ,
                  id_sdd            ,
                  resul_opera       ,
                  diagnostico       ,
                  folio_dispersion  ,
                  folio_liquidacion
                  )
      VALUES     (
                  v_id_dac_solicitud  , 
                  v_id_derechohabiente,
                  p_folio_integra     ,
                  v_num_credito       ,
                  v_nss               ,
                  v_periodo_pago      ,
                  v_imp_amortizacion  ,
                  v_folio_sua         ,
                  v_nrp               ,
                  v_id_sdd            ,
                  v_resul_opera       ,
                  v_diagnostico       ,
                  v_folio_ajuste      ,    -- debe ser el folio del ajuste de abono de dispersion 
                  v_folio_liquidacion
                 );

      LET v_tot_solicitudes = v_tot_solicitudes + 1;
      
      IF v_resul_opera = 1 THEN 
         LET v_tot_aceptadas = v_tot_aceptadas + 1;
      ELSE
         LET v_tot_rechazadas = v_tot_rechazadas + 1;
      END IF 
   END FOREACH;

    --trace "Al actualizar glo_ctr_archivo";
    LET err_txt = "Al actualizar glo_ctr_archivo";
    -- Se asigna el folio al archivo y se indica que ha sido integrado
    UPDATE glo_ctr_archivo
    SET    folio = p_folio_integra, 
           estado = 2 -- integrado
    WHERE  proceso_cod    = 2601
    AND    opera_cod      = 1 -- archivo cargado
    AND    estado         = 1; -- etapa de carga

    UPDATE bat_ctr_operacion 
    SET    folio       = p_folio_integra
    WHERE  proceso_cod = 2601
    AND    opera_cod   = 2
    AND    pid         = p_pid;   

   LET err_txt = "Total de Solicitudes: "||v_tot_solicitudes;

   UPDATE STATISTICS FOR TABLE dac_det_solicitud;
   UPDATE STATISTICS FOR TABLE dac_rch_archivo;

   RETURN v_resultado,
          v_isam_err,
          err_txt,
          v_tot_solicitudes, 
          v_tot_aceptadas,
          v_tot_rechazadas;
END FUNCTION
;


