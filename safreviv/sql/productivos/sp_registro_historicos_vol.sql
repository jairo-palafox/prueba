






CREATE PROCEDURE "safreviv".sp_registro_historicos_vol(p_folio       DECIMAL(9,0),
                                            p_pid         DECIMAL(9,0),
                                            p_proceso_cod SMALLINT)
                                           
RETURNING SMALLINT, 
          SMALLINT, 
          VARCHAR(255), 
          CHAR(11)

-- variables de las tablas temporales
-- encabezado: tmp_pag_cza_vol
DEFINE v_tmp_pag_cza_apvol_tpo_registro CHAR(2);
DEFINE v_tmp_pag_cza_apvol_proceso_cod  SMALLINT;
DEFINE v_tmp_pag_cza_apvol_opera_cod    SMALLINT;
DEFINE v_tmp_pag_cza_apvol_f_archivo    CHAR(8);

-- detalle: tmp_pag_det_vol
--DEFINE v_tmp_pag_det_apvol_tpo_registro   CHAR(2);
DEFINE v_tmp_pag_det_apvol_id_referencia    DECIMAL(9,0);
DEFINE v_tmp_pag_det_apvol_nss              CHAR(11);
DEFINE v_tmp_pag_det_apvol_cve_entidad      CHAR(6);
DEFINE v_tmp_pag_det_apvol_id_pago          CHAR(10);
DEFINE v_tmp_pag_det_apvol_f_pago           CHAR(8);
DEFINE v_tmp_pag_det_apvol_tpo_pago         CHAR(2);
DEFINE v_tmp_pag_det_apvol_curp             CHAR(18);
DEFINE v_tmp_pag_det_apvol_nom_trabajador   CHAR(50);
DEFINE v_tmp_pag_det_apvol_imp_ap_vol       DECIMAL(12,2);
--DEFINE v_tmp_pag_det_apvol_result_operacion CHAR(2);

-- detalle: tmp_pag_sum_vol
DEFINE v_tmp_pag_sum_apvol_tpo_registro    CHAR(02);
DEFINE v_tmp_pag_sum_apvol_num_reg_detalle DECIMAL(10,0);
DEFINE v_tmp_pag_sum_apvol_tot_ap_vol      DECIMAL(22,2);


-- tabla encabezado destino
-- pag_cza_vol
DEFINE v_pag_cza_apvol_folio        DECIMAL(9,0);
DEFINE v_pag_cza_apvol_tpo_registro CHAR(2);
DEFINE v_pag_cza_apvol_proceso_cod  SMALLINT;
DEFINE v_pag_cza_apvol_opera_cod    SMALLINT;
DEFINE v_pag_cza_apvol_f_archivo    DATE;

-- tabla detalle destino
-- pag_det_vol
--DEFINE v_pag_det_apvol_folio            DECIMAL(9,0);
DEFINE v_pag_det_apvol_id_referencia      DECIMAL(9,0);
DEFINE v_pag_det_apvol_id_derechohabiente DECIMAL(9,0);
DEFINE v_pag_det_apvol_cve_entidad        CHAR(6);
DEFINE v_pag_det_apvol_id_pago            CHAR(10);
DEFINE v_pag_det_apvol_f_pago             DATE;
DEFINE v_pag_det_apvol_tpo_pago           CHAR(2);
DEFINE v_pag_det_apvol_curp               CHAR(18);
DEFINE v_pag_det_apvol_nom_trabajador     CHAR(50);
DEFINE v_pag_det_apvol_imp_ap_vol         DECIMAL(12,2);
DEFINE v_pag_det_apvol_result_operacion   CHAR(2);
DEFINE v_pag_det_apvol_ind_cambio_nss     SMALLINT;
DEFINE v_pag_det_apvol_f_cambio_nss       DATE;

-- tabla detalle destino
-- pag_sum_vol
DEFINE v_pag_sum_apvol_folio           DECIMAL(9,0);
DEFINE v_pag_sum_apvol_tpo_registro    CHAR(02);
DEFINE v_pag_sum_apvol_num_reg_detalle DECIMAL(10,0);
DEFINE v_pag_sum_apvol_tot_ap_vol      DECIMAL(22,2);


DEFINE v_conteo       SMALLINT;
DEFINE v_conteo_saldo SMALLINT; -- cuentas que tienen saldo
DEFINE v_sum_saldo    DECIMAL(20,2); -- suma del saldo

DEFINE v_tmp_num_registros     INTEGER;
DEFINE v_tmp_sum_tot_aport     DECIMAL(22,2);
DEFINE v_tmp_num_registros_sum INTEGER;
DEFINE v_tmp_sum_tot_aport_sum DECIMAL(22,2);

DEFINE v_monto_acciones     DECIMAL(12,2);DEFINE v_siafore            SMALLINT; -- Fondo 11

--DEFINE v_tmp_pag_det_apvol_nss CHAR(11);
DEFINE v_nss_validado CHAR(11);
   
 -- Control de Excepciones
DEFINE v_error_sql     SMALLINT;
DEFINE v_isam_error    SMALLINT;
DEFINE v_mensaje_error VARCHAR(255);

DEFINE v_error_en_sumario   SMALLINT; -- booleana para saber si hubo error en sumario

   -- manejo de excepciones
   ON EXCEPTION SET v_error_sql, 
                    v_isam_error, 
                    v_mensaje_error
      
      
      LET v_tmp_pag_det_apvol_nss = '';
      RETURN v_error_sql, 
             v_isam_error, 
             v_mensaje_error, 
             v_tmp_pag_det_apvol_nss;
   END EXCEPTION
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_registro_historicos_vol.trace';
   --TRACE 'Inicia el store procedure de registro historicos de SOLO INFONAVIT';
   --TRACE "Folio:"||p_folio;
   
   LET v_isam_error    = 0;
   LET v_error_sql     = 0;
   LET v_mensaje_error = 'Integración realizada correctamente';
   LET v_tmp_pag_det_apvol_nss = '';   
   LET v_siafore = 11;
   
   -- se asume que no hay error en sumario
   LET v_error_en_sumario = 0;
   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
      SET folio = p_folio,
          estado = 2 -- integrado
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = 1 -- archivo cargado
      AND estado      = 1; -- etapa de carga

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
      SET folio       = p_folio
    WHERE proceso_cod = p_proceso_cod 
      AND opera_cod   = 2
      AND pid         = p_pid;
   
   UPDATE bat_ctr_proceso
      SET folio = p_folio
    WHERE proceso_cod = p_proceso_cod
      AND pid = p_pid;
   
   
   -- se revisa el sumario contra el detalle
   -- DETALLE
   SELECT COUNT(*)
     INTO v_tmp_num_registros
     FROM safre_tmp:tmp_pag_det_apvol ;
   
   SELECT SUM(imp_ap_vol)
     INTO v_tmp_sum_tot_aport
     FROM safre_tmp:tmp_pag_det_apvol ;
     
   -- SUMARIO
   SELECT num_reg_detalle
     INTO v_tmp_num_registros_sum
     FROM safre_tmp:tmp_pag_sum_apvol ;
   
   SELECT tot_ap_vol
     INTO v_tmp_sum_tot_aport_sum
     FROM safre_tmp:tmp_pag_sum_apvol ;   
     

   -- con alguno que no coincida, es causa de rechazo de lote
   -- compara el numero de registros
   IF( v_tmp_num_registros <> v_tmp_num_registros_sum )THEN
      LET v_error_sql     = 10;
      LET v_mensaje_error = "@El numero de registros en detalle y en sumario no coinciden";
        
      RETURN v_error_sql, 
             v_isam_error, 
             v_mensaje_error, 
             v_tmp_pag_det_apvol_nss;
   END IF

   -- compara el numero de registros
   IF( v_tmp_sum_tot_aport <> v_tmp_sum_tot_aport_sum )THEN
      LET v_error_sql     = 20;
      LET v_mensaje_error = "@La suma del total de aportaciones en detalle y en sumario no coincide";
        
      RETURN v_error_sql, 
             v_isam_error, 
             v_mensaje_error, 
             v_tmp_pag_det_apvol_nss;
   END IF

   LET v_tmp_pag_det_apvol_nss = NULL;
   --TRACE "1";
   -- Registro del encabezado
   FOREACH SELECT tpo_registro,
                  proceso_cod,
                  opera_cod,
                  f_archivo         
               
             INTO v_tmp_pag_cza_apvol_tpo_registro,
                  v_tmp_pag_cza_apvol_proceso_cod,
                  v_tmp_pag_cza_apvol_opera_cod,
                  v_tmp_pag_cza_apvol_f_archivo
                  
           FROM safre_tmp:tmp_pag_cza_apvol

      -- se asignan los datos
      LET v_pag_cza_apvol_folio        = p_folio;
      LET v_pag_cza_apvol_tpo_registro = v_tmp_pag_cza_apvol_tpo_registro;
      LET v_pag_cza_apvol_proceso_cod  = v_tmp_pag_cza_apvol_proceso_cod;
      LET v_pag_cza_apvol_opera_cod    = v_tmp_pag_cza_apvol_opera_cod;
      LET v_pag_cza_apvol_f_archivo    = v_tmp_pag_cza_apvol_f_archivo[5,6]||"/"||v_tmp_pag_cza_apvol_f_archivo[7,8]||"/"||v_tmp_pag_cza_apvol_f_archivo[1,4];      
      
      -- se inserta en la tabla de encabezado
      INSERT INTO pag_cza_apvol 
        (folio,
         tpo_registro,
         proceso_cod,
         opera_cod,
         f_archivo)
      VALUES 
        (v_pag_cza_apvol_folio,
         v_pag_cza_apvol_tpo_registro,
         v_pag_cza_apvol_proceso_cod,
         v_pag_cza_apvol_opera_cod,
         v_pag_cza_apvol_f_archivo );
   END FOREACH;
   --TRACE "2";
   -- Registro del sumario
   FOREACH SELECT tpo_registro,
                  num_reg_detalle,
                  tot_ap_vol/100
               
             INTO v_tmp_pag_sum_apvol_tpo_registro,
                  v_tmp_pag_sum_apvol_num_reg_detalle,
                  v_tmp_pag_sum_apvol_tot_ap_vol
                  
           FROM safre_tmp:tmp_pag_sum_apvol

      -- se asignan los datos
      LET v_pag_sum_apvol_folio           = p_folio;
      LET v_pag_sum_apvol_tpo_registro    = v_tmp_pag_sum_apvol_tpo_registro;
      LET v_pag_sum_apvol_num_reg_detalle = v_tmp_pag_sum_apvol_num_reg_detalle;
      LET v_pag_sum_apvol_tot_ap_vol      = v_tmp_pag_sum_apvol_tot_ap_vol;

      -- se inserta en la tabla de encabezado
      INSERT INTO pag_sum_apvol 
                 (folio,
                  tpo_registro,
                  num_reg_detalle,
                  tot_ap_vol)
          VALUES (v_pag_sum_apvol_folio,
                  v_pag_sum_apvol_tpo_registro,
                  v_pag_sum_apvol_num_reg_detalle,
                  v_pag_sum_apvol_tot_ap_vol);
   END FOREACH;
   --TRACE "3";
   --se seleccionan los registros de la tabla temporal de detalle	
   FOREACH SELECT id_referencia,
                  nss,
                  cve_entidad,
                  id_pago,
                  f_pago,
                  tpo_pago,
                  curp,
                  nom_trabajador,
                  imp_ap_vol/100
             INTO v_tmp_pag_det_apvol_id_referencia,
                  v_tmp_pag_det_apvol_nss,
                  v_tmp_pag_det_apvol_cve_entidad,
                  v_tmp_pag_det_apvol_id_pago,
                  v_tmp_pag_det_apvol_f_pago,
                  v_tmp_pag_det_apvol_tpo_pago,
                  v_tmp_pag_det_apvol_curp,
                  v_tmp_pag_det_apvol_nom_trabajador,
                  v_tmp_pag_det_apvol_imp_ap_vol
                  --v_tmp_pag_det_apvol_result_operacion
             FROM safre_tmp:tmp_pag_det_apvol    
     
     
                                                                 
     LET v_pag_det_apvol_id_referencia    = v_tmp_pag_det_apvol_id_referencia ;
     LET v_pag_det_apvol_cve_entidad      = v_tmp_pag_det_apvol_cve_entidad;
     LET v_pag_det_apvol_id_pago          = v_tmp_pag_det_apvol_id_pago;
     LET v_pag_det_apvol_f_pago           = v_tmp_pag_det_apvol_f_pago[5,6]||"/"||v_tmp_pag_det_apvol_f_pago[7,8]||"/"||v_tmp_pag_det_apvol_f_pago[1,4];
     LET v_pag_det_apvol_tpo_pago         = v_tmp_pag_det_apvol_tpo_pago;
     LET v_pag_det_apvol_curp             = v_tmp_pag_det_apvol_curp;
     LET v_pag_det_apvol_nom_trabajador   = v_tmp_pag_det_apvol_nom_trabajador;
     
     -- Se recibe en pesos y se pasa a aivs
     -- Se comenta hasta revisar con Hamir si se convierte a aivs 21-may-2013
     -- y sel iguala la variable v_pag_det_apvol_imp_ap_vol = v_tmp_pag_det_apvol_imp_ap_vol
     
-----     LET v_monto_acciones = fn_consulta_precio_fondo (v_tmp_pag_det_apvol_imp_ap_vol, -- importe aportacion voluntaria
-----                                                      v_pag_det_apvol_f_pago  , -- se considera fecha de pago
-----                                                      v_siafore ) ; -- fondo 11
-----     LET v_pag_det_apvol_imp_ap_vol       = v_monto_acciones;

     LET v_pag_det_apvol_imp_ap_vol     = v_tmp_pag_det_apvol_imp_ap_vol;
     LET v_pag_det_apvol_ind_cambio_nss = 0;
     LET v_pag_det_apvol_f_cambio_nss   = TODAY;
     
     EXECUTE FUNCTION fn_glo_valida_numerico(v_tmp_pag_det_apvol_nss) INTO v_error_sql,v_mensaje_error,v_nss_validado;
     
     IF ( v_error_sql <> 0 )THEN
        LET v_error_sql     = -100;
        LET v_mensaje_error = "@NSS no valido";
        
        RETURN v_error_sql, 
               v_isam_error, 
               v_mensaje_error, 
               v_tmp_pag_det_apvol_nss;
     END IF

     --se selecciona el id_derechohabioente de afi_derechohabiente
     SELECT id_derechohabiente 
       INTO v_pag_det_apvol_id_derechohabiente
       FROM afi_derechohabiente 
      WHERE nss = v_tmp_pag_det_apvol_nss ;
      
     LET v_pag_det_apvol_result_operacion = "01";
     
     --SI EL ID_DERECHOHABIENTE NO EXISTE  result_operacion = 02 
     IF( v_pag_det_apvol_id_derechohabiente IS NULL )THEN

       	-- Apertura de cuenta
        LET v_pag_det_apvol_id_derechohabiente = fn_apertura_cuenta_pag( v_tmp_pag_det_apvol_nss , 
                                                                         v_tmp_pag_det_apvol_curp ,  
                                                                         NULL,
                                                                         "0",
                                                                         v_tmp_pag_det_apvol_nom_trabajador,  --Nombre del trabajador
                                                                         "I",
                                                                         0,
                                                                         0,  
                                                                         p_folio,
                                                                         "R" ); -- recaudacion
     END IF 

     -- se insertan los registros a la tabla del detalle 
     INSERT INTO pag_det_apvol
                 (folio,
                  id_referencia,
                  id_derechohabiente,
                  cve_entidad,
                  id_pago,
                  f_pago,
                  tpo_pago,
                  curp,
                  nom_trabajador,
                  imp_ap_vol,
                  result_operacion,
                  ind_cambio_nss,
                  f_cambio_nss)
                  
          VALUES (p_folio,
                  v_pag_det_apvol_id_referencia,
                  v_pag_det_apvol_id_derechohabiente,
                  v_pag_det_apvol_cve_entidad,
                  v_pag_det_apvol_id_pago,
                  v_pag_det_apvol_f_pago,
                  v_pag_det_apvol_tpo_pago,
                  v_pag_det_apvol_curp,
                  v_pag_det_apvol_nom_trabajador,
                  v_pag_det_apvol_imp_ap_vol,
                  v_pag_det_apvol_result_operacion,
                  v_pag_det_apvol_ind_cambio_nss,
                  v_pag_det_apvol_f_cambio_nss);
   END FOREACH ;
   
   

   UPDATE STATISTICS FOR TABLE pag_cza_apvol ;
   UPDATE STATISTICS FOR TABLE pag_det_apvol ;
   UPDATE STATISTICS FOR TABLE pag_sum_apvol ;
   
   -- si no hubo error, se indica que el proceso termino correctamente
   LET v_tmp_pag_det_apvol_nss = "";
   LET v_error_sql     = 0;
   LET v_isam_error    = 0;
   LET v_mensaje_error = "El proceso de integración de aportaciónes voluntarias terminó correctamente";
   
   RETURN v_error_sql, 
          v_isam_error, 
          v_mensaje_error, 
          v_tmp_pag_det_apvol_nss;
          
END PROCEDURE;


