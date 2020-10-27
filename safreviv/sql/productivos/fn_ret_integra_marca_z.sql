






CREATE FUNCTION "safreviv".fn_ret_integra_marca_z(   p_usuario_cod    CHAR(20)
                                         , p_folio          DECIMAL(9,0)
                                         , p_nombre_archivo VARCHAR(40)
                                         , p_pid            DECIMAL(9,0)
                                         , p_proceso_cod    SMALLINT
                                       )
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)
   

-- campos de la tabla de detalle de marca Z (sin filler)
DEFINE tmp_ret_det_nss                         CHAR(11)          ;
DEFINE tmp_ret_det_causal_retiro               CHAR(1)           ;
DEFINE tmp_ret_det_curp                        CHAR(18)          ;
DEFINE tmp_ret_det_nombre_datamart             CHAR(50)          ;
DEFINE tmp_ret_det_sec_pension                 CHAR(02)          ;
DEFINE tmp_ret_det_tpo_movto                   CHAR(1)           ;
DEFINE tmp_ret_det_f_solicitud                 CHAR(10)          ;
DEFINE tmp_ret_det_regimen                     CHAR(2)           ;
DEFINE tmp_ret_det_semanas_cot                 CHAR(3)           ;
DEFINE tmp_ret_det_aivs_97                     CHAR(14)          ;
DEFINE tmp_ret_det_aivs_92                     CHAR(14)          ;
DEFINE tmp_ret_det_cve_afore                   CHAR(3)           ;

-- detalle de la tabla historica/integrada de aclaraciones del fondo de ahorro
-- ret_det_marca_Z
DEFINE ret_det_id_solicitud                    DECIMAL(9,0)      ;
DEFINE ret_det_folio                           DECIMAL(9,0)      ;
DEFINE ret_det_id_derechohabiente              DECIMAL(9,0)      ;
DEFINE ret_det_estado_solicitud                SMALLINT          ;
DEFINE ret_det_cod_rechazo                     SMALLINT          ;
DEFINE ret_det_nss                             CHAR(11)          ;
DEFINE ret_det_causal_retiro                   CHAR(1)           ;
DEFINE ret_det_curp                            CHAR(18)          ;
DEFINE ret_det_nombre_datamart                 CHAR(50)          ;
DEFINE ret_det_sec_pension                     CHAR(02)          ;
DEFINE ret_det_tpo_movto                       CHAR(1)           ;
DEFINE ret_det_f_solicitud                     CHAR(10)          ;
DEFINE ret_det_regimen                         CHAR(2)           ;
DEFINE ret_det_semanas_cot                     CHAR(3)           ;
DEFINE ret_det_aivs_97                         CHAR(14)          ;
DEFINE ret_det_aivs_92                         CHAR(14)          ;
DEFINE ret_det_cve_afore                       CHAR(3)           ;

-- variables de soporte al proceso
DEFINE v_id_derechohabiente                    DECIMAL(9,0);
DEFINE v_estado_solicitud                      CHAR(3);
DEFINE v_cod_rechazo                           SMALLINT;
DEFINE v_nss                                   CHAR(11);
DEFINE v_nombre                                CHAR(50);
DEFINE v_ocurrencias                           SMALLINT; -- cantidad registros por busqueda
DEFINE v_marca_z                               SMALLINT;  -- marca para Marca Z 814
DEFINE v_id_solicitud                          DECIMAL(9,0);

-- =============================================================================
                                               
-- para rechazos                               
DEFINE v_b_rechazo_detalle                     SMALLINT;

-- RECUPERADOS                                    
                                               

DEFINE v_numero_registros                      DECIMAL(9,0);
                                               
DEFINE v_motivo_rechazo_1                      SMALLINT;
DEFINE v_motivo_rechazo_2                      SMALLINT;
DEFINE v_motivo_rechazo_3                      SMALLINT;
-- arreglo de codigos de rechazo               
DEFINE v_codigos_rechazo                       CHAR(30); -- los codigos van de tres en tres
DEFINE v_indice_codigos_rechazo                SMALLINT; 
                                               
-- conteo de rechazos e inserciones            
DEFINE v_reg_det_insertados                    DECIMAL(9,0); -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados                    DECIMAL(9,0); -- total de registros de detalle rechazados

DEFINE v_resultado                             SMALLINT;

-- codigos de error en detalle
DEFINE v_error_det_nss_no_encontrado           SMALLINT;

-- para marcar las cuentas
DEFINE v_i_estado_marca                        INTEGER;
DEFINE v_marca_fondo_ahorro                    INTEGER; -- 802 de acuerdo a catalogo

-- Control de Excepciones
DEFINE v_si_resultado                          INTEGER; --SMALLINT;
DEFINE sql_err                                 INTEGER;
DEFINE isam_err                                INTEGER;
DEFINE err_txt                                 VARCHAR(250);
DEFINE v_c_msj                                 VARCHAR(250);

   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, tmp_ret_det_nss;
   END EXCEPTION
     
   --SET DEBUG FILE TO "/safreviv_int/BD/fn_ret_integra_fondo_ahorro.trace";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_det_insertados              = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados              = 0; -- total de registros de detalle rechazados
   LET v_marca_z                         = 814;
   LET v_nss                             = "";
   LET v_nombre                          = "";
   LET tmp_ret_det_nss                   = "";


   
   -- se asume que el proceso termina bien
   LET v_si_resultado  = 0;
   LET isam_err        = 0;
   LET v_c_msj         = 'El proceso finalizó exitosamente.';

    
   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado                = 49; -- NSS NO ENCONTRADO

   LET v_i_estado_marca     = 0;
   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio  = p_folio,
          estado = 2             -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio,
          nom_archivo = p_nombre_archivo
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   -- Agregar folio a proceso
   UPDATE bat_ctr_proceso
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    pid         = p_pid;
   

   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;

   -- se inicia la variable que almacenaria el id_solicitud
   LET v_id_solicitud             = 0;
   LET v_id_derechohabiente       = 0;
   
   
   -- se asume que no hay rechazos en el detalle del archivo
   LET v_b_rechazo_detalle    = 0;    

   -- se obtienen los datos del detalle
   FOREACH
   SELECT
       nss            ,
       causal_retiro  ,
       curp           ,
       nombre_datamart,
       sec_pension    ,
       tpo_movto      ,
       f_solicitud    ,
       regimen        ,
       semanas_cot    ,
       aivs_97        ,
       aivs_92        ,
       cve_afore
   INTO
       tmp_ret_det_nss             ,
       tmp_ret_det_causal_retiro   ,
       tmp_ret_det_curp            ,
       tmp_ret_det_nombre_datamart ,
       tmp_ret_det_sec_pension     ,
       tmp_ret_det_tpo_movto       ,
       tmp_ret_det_f_solicitud     ,
       tmp_ret_det_regimen         ,
       tmp_ret_det_semanas_cot     ,
       tmp_ret_det_aivs_97         ,
       tmp_ret_det_aivs_92         ,
       tmp_ret_det_cve_afore
   FROM safre_tmp:tmp_ret_det_marca_z  --tabla para almacenar el detalle

   LET v_id_solicitud = 0;
   -- se obtiene el id_solicitud
   SELECT seq_ret_det_marca_z_id_solicitud.NEXTVAL
   INTO   v_id_solicitud
   FROM   systables
   WHERE  tabid = 1;
       
   LET v_indice_codigos_rechazo = 1;
   LET v_id_derechohabiente     = NULL ;  
   --- Busca RFC en afi_fondo72
   SELECT COUNT(id_derechohabiente)
   INTO   v_ocurrencias
   FROM   afi_derechohabiente
   WHERE  nss = tmp_ret_det_nss;
             
   --- si lo encuentra se obtiene el NSS y el Nombre, en caso contrario lo rechaza
   IF v_ocurrencias = 1 THEN
       SELECT id_derechohabiente
       INTO   v_id_derechohabiente
       FROM   afi_derechohabiente
       WHERE  nss = tmp_ret_det_nss;
       --marca la cuenta de cargo
       EXECUTE FUNCTION fn_marca_cuenta(
               v_id_derechohabiente
              ,v_marca_z  -- marca Z
              ,v_id_solicitud
              ,p_folio     -- folio se asignara en la solicitd
              ,0     -- estado marca
              ,0     -- codigo de rechazo
              ,0     -- marca de la causa
              ,NULL  -- fecha de la causa
              ,USER  --usuario
              ,p_proceso_cod) --proceso_cod
          INTO v_i_estado_marca; 
       IF v_i_estado_marca <> 0 THEN 
           --Problemas con la marca
           LET v_estado_solicitud = 100;   -- Solicitud rechazada por registro existente
           LET v_cod_rechazo      = 103;   -- Problemas al marcar
       ELSE 
           LET v_estado_solicitud = 0;   -- Solicitud aceptada, cuenta marcada
           LET v_cod_rechazo      = 0;   -- Cuenta marcada
       END IF
   ELSE 
       LET v_estado_solicitud = 100;     -- Solicitud rechazada por registro existente
       LET v_cod_rechazo      = v_error_det_nss_no_encontrado;   -- No existe el NSS
   END IF

   LET ret_det_id_solicitud             = v_id_solicitud               ;
   LET ret_det_folio                    = p_folio                      ;
   LET ret_det_id_derechohabiente       = v_id_derechohabiente         ;
   LET ret_det_estado_solicitud         = v_estado_solicitud           ;
   LET ret_det_cod_rechazo              = v_cod_rechazo                ;
   LET ret_det_nss                      = tmp_ret_det_nss              ;
   LET ret_det_causal_retiro            = tmp_ret_det_causal_retiro    ;
   LET ret_det_curp                     = tmp_ret_det_curp             ;
   LET ret_det_nombre_datamart          = tmp_ret_det_nombre_datamart  ;
   LET ret_det_sec_pension              = tmp_ret_det_sec_pension      ;
   LET ret_det_tpo_movto                = tmp_ret_det_tpo_movto        ;
   LET ret_det_f_solicitud              = tmp_ret_det_f_solicitud      ;
   LET ret_det_regimen                  = tmp_ret_det_regimen          ;
   LET ret_det_semanas_cot              = tmp_ret_det_semanas_cot      ;
   LET ret_det_aivs_97                  = tmp_ret_det_aivs_97          ;
   LET ret_det_aivs_92                  = tmp_ret_det_aivs_92          ;
   LET ret_det_cve_afore                = tmp_ret_det_cve_afore        ;

   INSERT INTO ret_det_marca_z
              (id_solicitud,
               folio,
               id_derechohabiente,
               estado_solicitud,
               cod_rechazo,
               nss,
               causal_retiro,
               curp,
               nombre_datamart,
               sec_pension,
               tpo_movto,
               f_solicitud,
               regimen,
               semanas_cot,
               aivs_97,
               aivs_92,
               cve_afore)
       VALUES (ret_det_id_solicitud,
               ret_det_folio,
               ret_det_id_derechohabiente,
               ret_det_estado_solicitud,
               ret_det_cod_rechazo,
               ret_det_nss,
               ret_det_causal_retiro,
               ret_det_curp,
               ret_det_nombre_datamart,
               ret_det_sec_pension,
               ret_det_tpo_movto,
               ret_det_f_solicitud,
               ret_det_regimen,
               ret_det_semanas_cot,
               ret_det_aivs_97,
               ret_det_aivs_92,
               ret_det_cve_afore);
   LET v_reg_det_insertados = v_reg_det_insertados + 1;
   END FOREACH;

   -- se actualizan las estadisticas
   UPDATE STATISTICS FOR TABLE ret_det_marca_z;

   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_det_nss;
END FUNCTION;


