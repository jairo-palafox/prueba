






CREATE FUNCTION "safreviv".fn_dpe_integra_det_infonavit(p_usuario_cod CHAR(20),
                                             p_pid DECIMAL(9,0),
                                             p_nombre_archivo CHAR(18),
                                             p_folio DECIMAL(10),
                                             p_proceso_cod SMALLINT) 
  RETURNING INTEGER, CHAR(200), SMALLINT, SMALLINT, SMALLINT, SMALLINT
-- Variables utilizadas para el detalle de devoluciones
-- de aportaciones pagadas en exceso solo INFONAVIT
DEFINE v_det_tpo_registro                DECIMAL(1,0);
DEFINE v_det_nss                         CHAR(11);
DEFINE v_det_credito                     DECIMAL(10,0);
DEFINE v_det_periodo_pago                CHAR(6);
DEFINE v_det_fec_pago                    CHAR(8);
DEFINE v_det_date_fec_pago               DATE;
DEFINE v_det_enti_recaudadora            DECIMAL(3,0);
DEFINE v_det_nrp                         CHAR(11);
DEFINE v_det_aportacion_registrada       DECIMAL(12,2);
DEFINE v_det_amortiza_registrada         DECIMAL(12,2);
DEFINE v_det_aportacion_solicitada       DECIMAL(12,2);
DEFINE v_det_amortiza_solicitada         DECIMAL(12,2);
DEFINE v_det_llave_sdd                   DECIMAL(6,0);
DEFINE v_det_folio_sua                   DECIMAL(6,0);
DEFINE v_det_cve_rechazo                 DECIMAL(5,0);
DEFINE v_id_derechohabiente              DECIMAL(9,0);
DEFINE v_det_estado_solicitud            SMALLINT;
-- Variables que se retornan
DEFINE v_i_resultado                     SMALLINT;
DEFINE v_si_numero_solicitudes_totales   SMALLINT;
DEFINE v_si_numero_solicitudes_aceptadas SMALLINT;
DEFINE v_si_total_trabaja                SMALLINT;
DEFINE v_si_status_detalle_trabaj        SMALLINT;
-- Control de Excepciones
DEFINE sql_err                           INTEGER;
DEFINE isam_err                          INTEGER;
DEFINE err_txt                           CHAR(200);
--
DEFINE v_si_resultado                    SMALLINT;
DEFINE v_d_id_referencia                 SMALLINT;
-- Variable para marca de cuenta
DEFINE v_i_estado_marca                  INTEGER;
-- Variables para verificar el saldo del nss
DEFINE v_subcuenta                       SMALLINT; -- subcuenta 4
DEFINE v_subcuenta44                     SMALLINT; -- subcuenta 44
DEFINE v_saldo_pesos                     DECIMAL(16,6);
DEFINE v_d_saldo_disponible_aivs         DECIMAL(16,6);
DEFINE v_resultado_saldo                 SMALLINT;

DEFINE v_sub_cuenta                      SMALLINT;      
DEFINE v_fondo_inversion                 SMALLINT; 
DEFINE v_acciones                        DECIMAL(16,6);        
DEFINE v_pesos                           DECIMAL(16,6);


   ON EXCEPTION SET sql_err, isam_err
      LET v_i_resultado = sql_err;
      LET v_si_numero_solicitudes_totales = 0;
      LET v_si_numero_solicitudes_aceptadas = 0;
      LET v_si_total_trabaja = 0;
      
      RETURN v_i_resultado,
             err_txt,
             v_si_numero_solicitudes_totales, 
             v_si_numero_solicitudes_aceptadas,
             v_si_status_detalle_trabaj, 
             v_si_total_trabaja;
   END EXCEPTION


-- Variables que almacenan informacion para su validacion
   LET v_det_tpo_registro                = NULL;              
   LET v_det_nss                         = NULL;              
   LET v_det_credito                     = NULL;              
   LET v_det_periodo_pago                = NULL;              
   LET v_det_fec_pago                    = NULL;              
   LET v_det_date_fec_pago               = NULL;              
   LET v_det_enti_recaudadora            = NULL;              
   LET v_det_nrp                         = NULL;              
   LET v_det_aportacion_registrada       = NULL;              
   LET v_det_amortiza_registrada         = NULL;              
   LET v_det_aportacion_solicitada       = NULL;              
   LET v_det_amortiza_solicitada         = NULL;              
   LET v_det_llave_sdd                   = NULL;              
   LET v_det_folio_sua                   = NULL;              
   LET v_det_cve_rechazo                 = NULL;
   LET v_id_derechohabiente              = NULL;
   LET v_det_estado_solicitud            = 1; -- Acepatada
   LET v_i_resultado                     = 0;
   LET v_si_numero_solicitudes_totales   = 0;
   LET v_si_numero_solicitudes_aceptadas = 0;
   LET v_si_total_trabaja                = 0;
   LET v_si_status_detalle_trabaj        = 0;
   LET v_d_id_referencia                 = 0;
   LET v_si_resultado                    = 0;
   LET sql_err                           = NULL;
   LET isam_err                          = NULL;
   LET err_txt                           = NULL;
   LET v_subcuenta                       = 4; -- Valor de la subcuenta temporal
   LET v_subcuenta44                     = 44; -- Valor de la subcuenta temporal
   LET v_saldo_pesos                     = 0;
   LET v_d_saldo_disponible_aivs         = 0;
   LET v_resultado_saldo                 = 0;
   
   LET v_sub_cuenta                      = 0;      
   LET v_fondo_inversion                 = 0;
   LET v_acciones                        = 0;
   LET v_pesos                           = 0;


   --SET DEBUG FILE TO "/home/safreviv/trace.dpe.integra_det_infonavit.txt";
   --TRACE ON;
   -- [Error]
   --trace "Al obtener maximo de id_dpe de dpe_sol_soloinfonavit";
   LET err_txt = "Al obtener maximo de id_dpe de dpe_sol_soloinfonavit";
   -- Selecciona la informacion insertada al cargar el archivo para su validacion
   -- detalle de devolucion de aportaciones pagadas en exceso solo INFONAVIT
   
   -- Verifica que el id_referencia no venga nullo
   -- en caso de ser contrario, se asigna el valor que trae
   
   -- [Error]
   --trace "Al recuperar datos detalle tmp_det_recauda_hipoteca";
   LET err_txt = "Al recuperar datos detalle tmp_det_recauda_hipoteca";
   
   FOREACH
      SELECT tpo_registro,
             nss,
             credito,
             periodo_pago,
             fec_pago,
             enti_recaudadora,
             nrp,
             aportacion_registrada/100,
             amortiza_registrada/100,
             aportacion_solicitada/100,
             amortiza_solicitada/100,
             llave_sdd,
             folio_sua,
             cve_rechazo
        INTO v_det_tpo_registro,
             v_det_nss,
             v_det_credito,
             v_det_periodo_pago,
             v_det_fec_pago,
             v_det_enti_recaudadora,
             v_det_nrp,
             v_det_aportacion_registrada, --Comparar contra el saldo y sólo afecta la subcuenta 44 fn_saldo_actual > 0 acepta comparar saldo = v_det_aportacion_registrada
             v_det_amortiza_registrada,
             v_det_aportacion_solicitada, --Se regresa el monto que devuelve el saldo_dia 
             v_det_amortiza_solicitada,
             v_det_llave_sdd,
             v_det_folio_sua,
             v_det_cve_rechazo
        FROM safre_tmp:tmp_det_recauda_hipoteca
        
      SELECT seq_dpe_sol_soloinfonavit.NEXTVAL
        INTO v_d_id_referencia
	      FROM SYSTABLES
	     WHERE tabname = "dpe_sol_soloinfonavit";
	     
      -- Contador de solicitudes de archivo
      LET v_si_numero_solicitudes_totales = v_si_numero_solicitudes_totales + 1;
      
      -- Asume que la solicitud viene aceptada. En caso que exista un rechazo
      -- Su valor cambia a 2.
      LET v_det_estado_solicitud = 1; -- Acepatada
                      
      --trace "Valida tipo de registro para el registro inicial 0";
      LET err_txt = "Valida tipo de registro para el registro inicial 0";
      
      IF (v_det_tpo_registro <> 2 OR v_det_tpo_registro IS NULL) THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,2,v_d_id_referencia,"No",84,
                                                  "Tipo registro -"||v_det_tpo_registro);
                                                  
         LET v_si_resultado = 1;
         LET v_det_estado_solicitud = 2; -- Rechazada
      END IF
      
      --trace "Valida el nss";
      LET err_txt = "Valida el nss";
      
      IF (v_det_nss IS NULL) THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,2,v_d_id_referencia,"No",85,
                                                  "NSS -"||v_det_nss);
         LET v_si_resultado = 1;
         LET v_det_estado_solicitud = 2; -- Rechazada
      END IF
            
      --trace "Periodo de pago";
      LET err_txt = "Periodo de pago";
      
      IF (v_det_periodo_pago IS NULL) THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,2,v_d_id_referencia,"No",87,
                                                  "Periodo de pago -"||v_det_periodo_pago);
         LET v_si_resultado = 1;
         LET v_det_estado_solicitud = 2; -- Rechazada
      END IF
      
      -- Corrige fecha de YYYYMMDD a MMDDYYY
      EXECUTE PROCEDURE sp_cambia_formato_fecha(v_det_fec_pago)
         INTO v_det_date_fec_pago;
      
      -- Valida la fecha de registro
      --trace "Fecha de pago";
      LET err_txt = "Fecha de pago";
      
      IF(v_det_date_fec_pago > TODAY OR v_det_date_fec_pago IS NULL)THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,2,v_d_id_referencia,"No",88,
                                                  "Fecha de pago -"||v_det_date_fec_pago);
         LET v_si_resultado = 1;
         LET v_det_estado_solicitud = 2; -- Rechazada
      END IF
      
      --Recupera el id_derechohabiente
      SELECT id_derechohabiente 
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = v_det_nss;
       
       -- Valida el id_derechohabiente
      --trace "En id_derechohabiente, no existe";
      LET err_txt = "En id_derechohabiente, no existe";
      
      IF(v_id_derechohabiente IS NULL)THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,2,v_d_id_referencia,"No",89,
                                                  "Id derechohabiente no existe -"||v_id_derechohabiente);
         LET v_si_resultado = 1;
         LET v_det_estado_solicitud = 2; -- Rechazada
      END IF
            
      --Obtiene el saldo del día 
      FOREACH 
         EXECUTE FUNCTION fn_saldo_actual(v_det_nss,44,TODAY)              
              INTO v_sub_cuenta,
                   v_fondo_inversion,
                   v_acciones,
                   v_pesos
      END FOREACH;
                               
      IF (v_pesos  > 0) THEN 
        IF (v_pesos < v_det_aportacion_registrada) THEN
           LET v_det_aportacion_solicitada = v_pesos;
        ELSE
           LET v_det_aportacion_solicitada = v_det_aportacion_registrada;        
        END IF
      ELSE
         EXECUTE PROCEDURE sp_dpe_inserta_rechazo(p_folio,2,v_d_id_referencia,"No",21,
                                                  "No hay saldo -"||v_id_derechohabiente);
         LET v_si_resultado = 1;
         LET v_det_estado_solicitud = 2; -- Rechazada
      END IF 
      
      --trace "Al insertar detalle en dpe_sol_soloinfonavit";
      LET err_txt = "Al insertar detalle en dpe_sol_soloinfonavit";
      
      INSERT INTO dpe_sol_soloinfonavit(id_dpe_referencia,
                                        folio,
                                        id_derechohabiente,
                                        nss,
                                        credito,
                                        periodo_pago,
                                        fecha_pago,
                                        ent_recaudadora,
                                        nrp,
                                        aportacion_reg,
                                        amortizacion_reg,
                                        aportacion_sol,
                                        amortizacion_sol,
                                        llave_sdd,
                                        folio_sua,
                                        clave_rechazo,
                                        estado_solicitud)
       VALUES(v_d_id_referencia,  
              p_folio,
              v_id_derechohabiente,
              v_det_nss,
              v_det_credito,
              v_det_periodo_pago,
              v_det_date_fec_pago,
              v_det_enti_recaudadora,
              v_det_nrp,
              v_det_aportacion_registrada,
              v_det_amortiza_registrada,
              v_det_aportacion_solicitada,
              v_det_amortiza_solicitada,
              v_det_llave_sdd,
              v_det_folio_sua,
              v_det_cve_rechazo,
              v_det_estado_solicitud);
      
      --trace "Al marcar cuenta como aceptada";
      LET err_txt = "Al marcar cuenta como aceptada";
      -- # [1.10 Marcar cuenta que fue aceptada en proceso de devolucion INFONAVIT]
      IF v_det_estado_solicitud = 1 THEN
         LET v_i_estado_marca = 0;
         LET v_i_resultado = 1;
         LET v_si_status_detalle_trabaj = 1;
         
         EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente,
                                          402, -- marca de INFONAVIT
                                          v_d_id_referencia,  
                                          p_folio,
                                          0, -- estado marca
                                          0, -- codigo de rechazo
                                          0, -- marca de la causa
                                          NULL, -- fecha de la causa
                                          p_usuario_cod,
                                          p_proceso_cod)
            INTO v_i_estado_marca;
      END IF
      
      -- Conteo de totales de solicitudes del archivo
      LET v_si_total_trabaja = v_si_total_trabaja + 1;
      
      -- Conteo de solicitudes aceptadas del archivo
      IF v_det_estado_solicitud = 1 THEN
         LET v_si_numero_solicitudes_aceptadas = v_si_numero_solicitudes_aceptadas + 1;
      END IF	
      
 END FOREACH;
 
    --trace "Al actualizar glo_ctr_archivo";
    LET err_txt = "Al actualizar glo_ctr_archivo";
    -- Se asigna el folio al archivo y se indica que ha sido integrado
    UPDATE glo_ctr_archivo
       SET folio = p_folio, 
           estado = 2 -- integrado
     WHERE proceso_cod    = 1005
       AND opera_cod      = 1 -- archivo cargado
       AND estado         = 1; -- etapa de carga
       
    UPDATE bat_ctr_operacion 
      SET folio       = p_folio
    WHERE proceso_cod = 1005
      AND opera_cod   = 2
      AND pid         = p_pid;   
   
       
   --trace " Patrones: "||v_si_numero_solicitudes_totales;
   LET err_txt = " Patrones: "||v_si_numero_solicitudes_totales;
   
   update statistics for table dpe_sol_soloinfonavit;
   
 RETURN v_i_resultado, err_txt, v_si_numero_solicitudes_totales, 
        v_si_numero_solicitudes_aceptadas, v_si_status_detalle_trabaj, 
        v_si_total_trabaja;

END FUNCTION
;


