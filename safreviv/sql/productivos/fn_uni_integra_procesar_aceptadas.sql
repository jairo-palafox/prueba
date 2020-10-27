






CREATE FUNCTION "safreviv".fn_uni_integra_procesar_aceptadas(p_usuario_cod    CHAR   (20), 
                                                  p_folio          DECIMAL(10), 
                                                  p_nombre_archivo CHAR   (18), 
                                                  p_pid            DECIMAL(9,0),
                                                  p_proceso_cod    SMALLINT) 
  RETURNING INTEGER, 
            INTEGER, 
            CHAR(200),
            CHAR(11)

-- Variables utilizadas para el detalle unificadora UNI solo IMSS

DEFINE v_det_nss_unificador            CHAR(11);
DEFINE v_date_fecha_unificacion        DATE;

-- Variables que se retornan
DEFINE v_i_resultado                   INTEGER;

-- Control de Excepciones
DEFINE sql_err                         INTEGER;
DEFINE isam_err                        INTEGER;
DEFINE err_txt                         CHAR(2000);
DEFINE nss_error                       CHAR(11);

-- Variable para marca de cuenta
DEFINE v_seq_cre_archivo               INTEGER;
DEFINE v_id_unificador_dor             DECIMAL(9,0);
DEFINE v_id_unificado_ado              DECIMAL(9,0);
DEFINE v_folio_unificacion_lote        DECIMAL(9,0);
DEFINE v_id_derechohabiente_dor        DECIMAL(9,0);
DEFINE v_id_derechohabiente_ado        DECIMAL(9,0);
DEFINE v_f_lote                        DATE;
DEFINE v_resp_datos_credito            SMALLINT;
DEFINE v_ctr_folio_archivo             DECIMAL(9,0);

DEFINE v_si_numero_solicitudes_totales INTEGER;
DEFINE v_total_de_cambio_cre           INTEGER;
DEFINE v_total_desmarca                INTEGER;  
DEFINE v_id_derecho_error              DECIMAL(9,0);

DEFINE v_ctr_id_cre_ctr_archivo	       DECIMAL(9,0) ;
DEFINE v_ctr_lote	                     SMALLINT     ;
DEFINE v_ctr_f_lote	                   DATE         ;
DEFINE v_ctr_id_proceso	               SMALLINT     ;
DEFINE v_ctr_operacion	               SMALLINT     ;
DEFINE v_ctr_nom_archivo	             CHAR(40)     ;
DEFINE v_ctr_tot_registros	           DECIMAL(10,0);
DEFINE v_ctr_tot_aceptados	           DECIMAL(10,0);
DEFINE v_ctr_tot_rechazados	           DECIMAL(10,0);
DEFINE v_ctr_tot_sin_origen	           DECIMAL(10,0);
DEFINE v_ctr_estado	                   SMALLINT     ;
DEFINE v_ctr_f_proceso	               DATE         ;
DEFINE v_ctr_usuario	                 CHAR(20)     ;

ON EXCEPTION SET sql_err, 
                 isam_err, 
                 err_txt
                 
   LET v_i_resultado                   = sql_err;
   LET v_si_numero_solicitudes_totales = 0;
   LET v_total_de_cambio_cre           = 0;
   LET v_total_desmarca                = 0;
   
   RETURN v_i_resultado, 
          isam_err, 
          err_txt,
          nss_error;
END EXCEPTION

-- Variables que almacenan informacion para su validacion
LET v_i_resultado                         = 0;

LET v_si_numero_solicitudes_totales       = 0;
LET v_total_de_cambio_cre                 = 0;
LET v_total_desmarca                      = 0;

LET sql_err                               = NULL;
LET isam_err                              = NULL;
LET err_txt                               = "Proceso finaliza correctamente";
LET v_resp_datos_credito                  = 0;
LET v_id_derecho_error                    = NULL;
LET nss_error                             = NULL;
LET p_nombre_archivo                      = "MIGRACION_UNI";

   --SET DEBUG FILE TO "/safreviv_int/uni/envio/trace.uni_integra_PROCESAR_aceptadas.txt";
   --TRACE ON;
---------------------------CTAS UNIFICADOR----------------------------- 

   LET err_txt = "Al recuperar datos detalle tmp_det_cta_unificadora_op21";
   
   FOREACH
      SELECT nss_unificador,
             to_date(fecha_unificacion,'%Y%m%d')
      INTO   v_det_nss_unificador,
             v_date_fecha_unificacion
      FROM   safre_mig:tmp_det_unificador
                
      UPDATE uni_det_unificador
      SET    f_notificacion = v_date_fecha_unificacion
      WHERE  nss_unificador = v_det_nss_unificador
      AND    folio_unificacion = 3339
      AND    estado_familia = 1;
           
      LET v_si_numero_solicitudes_totales = v_si_numero_solicitudes_totales + 1;
   END FOREACH;

-----------------MUEVE INDICADORES DE CREDITO Y DESMARCA CUENTA------------------  
   SELECT seq_cre_archivo.NEXTVAL
     INTO v_seq_cre_archivo
     FROM systables
    WHERE tabname = "cre_ctr_archivo";
    
    EXECUTE FUNCTION fn_genera_folio(p_proceso_cod,
                                     22,
                                     p_usuario_cod)
    INTO v_ctr_folio_archivo;
     
   SELECT f_actualiza               
   INTO   v_f_lote
   FROM   safre_mig:glo_folio    
   WHERE  folio = 3339;
    
  --Invoca rutina para indicadores
   FOREACH 
      SELECT dor.id_unificador,
             ado.id_unificado,
             dor.folio_unificacion,
             dor.id_derechohabiente,
             ado.id_derechohabiente
        INTO v_id_unificador_dor,
             v_id_unificado_ado,
             v_folio_unificacion_lote,
             v_id_derechohabiente_dor,
             v_id_derechohabiente_ado
        FROM uni_det_unificador dor,
             uni_det_unificado ado
       WHERE dor.folio_unificacion = 3339
         AND dor.estado_familia = 1
         AND dor.f_notificacion IS NOT NULL
         AND ado.id_unificador = dor.id_unificador
                 
      EXECUTE FUNCTION fn_unifica_cuenta_procesar_aceptadas(v_id_unificador_dor,
                                                            v_id_unificado_ado,
                                                            v_id_derechohabiente_ado,
                                                            v_id_derechohabiente_dor,
                                                            v_folio_unificacion_lote,
                                                            v_f_lote,
                                                            p_pid,
                                                            p_nombre_archivo,
                                                            p_usuario_cod,
                                                            v_seq_cre_archivo,
                                                            v_ctr_folio_archivo)
                                    INTO v_resp_datos_credito,
                                         v_id_derecho_error;
   
      IF v_resp_datos_credito = 1 THEN
         LET v_total_de_cambio_cre = v_total_de_cambio_cre + 1;
      END IF
      
      EXECUTE FUNCTION fn_uni_posliquida_procesar_aceptadas(p_usuario_cod,           
                                                            v_folio_unificacion_lote,
                                                            p_proceso_cod, 
                                                            v_id_unificador_dor,
                                                            v_id_unificado_ado,
                                                            v_id_derechohabiente_ado,
                                                            v_id_derechohabiente_dor)
                                          INTO v_i_resultado, 
                                               isam_err, 
                                               err_txt;
      
      IF v_i_resultado = 0 THEN   
         LET v_total_desmarca = v_total_desmarca + 1;
      END IF
      
      UPDATE uni_det_unificador
      SET    diagnostico = 6
      WHERE  id_unificador = v_id_unificador_dor;
      
      UPDATE uni_det_unificado
      SET    diagnostico = 6
      WHERE  id_unificador = v_id_unificador_dor
      AND    id_unificado  = v_id_unificado_ado;
   END FOREACH;
  
   IF v_total_de_cambio_cre > 0 THEN
      LET v_ctr_id_cre_ctr_archivo	= v_seq_cre_archivo;      
      LET v_ctr_lote	              = v_ctr_folio_archivo;
      LET v_ctr_f_lote	            = v_f_lote;
      LET v_ctr_id_proceso	        = p_pid;
      LET v_ctr_operacion	          = 22;
      LET v_ctr_nom_archivo	        = p_nombre_archivo;
      LET v_ctr_tot_registros	      = v_total_de_cambio_cre; -- Por definir contabilizacion
      LET v_ctr_tot_aceptados	      = v_total_de_cambio_cre; -- Por definir contabilizacion
      LET v_ctr_tot_rechazados      = 0;
      LET v_ctr_tot_sin_origen      = 0;
      LET v_ctr_estado	            = 20;
      LET v_ctr_f_proceso	          = TODAY;
      LET v_ctr_usuario	            = p_usuario_cod;
      
      INSERT INTO cre_ctr_archivo (id_cre_ctr_archivo,
                                   folio_archivo,
                                   lote, 
                                   f_lote, 
                                   id_proceso, 
                                   operacion,
                                   nom_archivo,
                                   tot_registros,
                                   tot_aceptados,
                                   tot_rechazados, 
                                   tot_sin_origen, 
                                   estado, 
                                   f_proceso,
                                   usuario)
             VALUES (v_ctr_id_cre_ctr_archivo,
                     v_ctr_folio_archivo,
                     v_ctr_lote, 
                     v_ctr_f_lote, 
                     v_ctr_id_proceso, 
                     v_ctr_operacion,
                     v_ctr_nom_archivo,
                     v_ctr_tot_registros,
                     v_ctr_tot_aceptados,
                     v_ctr_tot_rechazados, 
                     v_ctr_tot_sin_origen, 
                     v_ctr_estado, 
                     v_ctr_f_proceso,
                     v_ctr_usuario);
      
   END IF          
                      
   LET err_txt = "Al actualizar glo_ctr_archivo";

   UPDATE safre_mig:glo_ctr_archivo
      SET folio = p_folio, 
          estado = 2 -- integrado
    WHERE proceso_cod    = p_proceso_cod
      AND opera_cod      = 1 -- archivo cargado
      AND estado         = 1; -- etapa de carga       
   
   UPDATE safre_mig:bat_ctr_operacion 
      SET folio       = p_folio,
          nom_archivo = p_nombre_archivo
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = 2
      AND pid         = p_pid;      
   
   LET err_txt = " Registros: "||v_si_numero_solicitudes_totales||
                 " Cambio de Credito: "||v_total_de_cambio_cre||
                 " Desmarca de Cuentas: "||v_total_desmarca;
   
   UPDATE statistics FOR TABLE safre_viv:uni_det_unificador;
   UPDATE statistics FOR TABLE safre_viv:uni_det_unificado;
   
   RETURN v_i_resultado,
          isam_err,
          err_txt,
          nss_error;
END FUNCTION
;


