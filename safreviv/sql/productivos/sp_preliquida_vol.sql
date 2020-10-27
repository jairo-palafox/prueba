






CREATE PROCEDURE "safreviv".sp_preliquida_vol(p_folio DECIMAL(9,0), p_usuario CHAR(20))

RETURNING SMALLINT, 
          SMALLINT, 
          VARCHAR(255)

--pag_det_apvol
DEFINE v_pag_det_apvol_folio              DECIMAL(9,0);
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


--pag_preliquida
DEFINE v_preliq_f_liquida       DATE;                     --f_liquida
DEFINE v_preliq_subcuenta       SMALLINT;                 --subcuenta
DEFINE v_preliq_fondo_inversion SMALLINT;                 --fondo_inversion
DEFINE v_preliq_movimiento      SMALLINT;                 --movimiento
DEFINE v_preliq_monto_acciones  DECIMAL(18,2);            --monto_acciones
DEFINE v_preliq_h_registro      DATETIME HOUR TO SECOND;  --h_registro
--DEFINE v_siafore                SMALLINT;

-- Control de Excepciones
DEFINE v_error_sql     SMALLINT;
DEFINE v_isam_error    SMALLINT;
DEFINE v_mensaje_error VARCHAR(255);


  --manejo de excepciones
  ON EXCEPTION SET v_error_sql, 
                   v_isam_error, 
                   v_mensaje_error

      RETURN v_error_sql, 
             v_isam_error, 
             v_mensaje_error;
  END EXCEPTION


   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_preliquida_vol.trace';
   --TRACE 'Inicia sp_preliquida_fc con Folio:' || p_folio;
   
   --asignación de variables    

   LET v_preliq_subcuenta       = 45;
   LET v_preliq_fondo_inversion = 11;
   LET v_preliq_movimiento      = 491;
   LET v_preliq_h_registro      = CURRENT HOUR TO SECOND;
   --LET v_siafore                = 10;
   LET v_preliq_monto_acciones  = 0;
                        
   
   --Inicia con la lectura de todos los registros de la tabla pag_det_apvol
   
   FOREACH SELECT folio,
                  id_referencia,
                  id_derechohabiente,
                  cve_entidad,
                  id_pago,
                  f_pago,
                  tpo_pago,
                  curp,
                  nom_trabajador,
                  imp_ap_vol,
                  result_operacion
             INTO v_pag_det_apvol_folio,
                  v_pag_det_apvol_id_referencia,
                  v_pag_det_apvol_id_derechohabiente,
                  v_pag_det_apvol_cve_entidad,
                  v_pag_det_apvol_id_pago,
                  v_pag_det_apvol_f_pago,
                  v_pag_det_apvol_tpo_pago,
                  v_pag_det_apvol_curp,
                  v_pag_det_apvol_nom_trabajador,
                  v_pag_det_apvol_imp_ap_vol,
                  v_pag_det_apvol_result_operacion
            FROM pag_det_apvol
           WHERE folio = p_folio
             --AND result_operacion = "01" 

-- SE COMENTA POR CORREO DE HAMIR DEL 21-ENE-2015
-- EL CUAL DICE QUE SE VALUAN LA AIVS CON LA
-- FECHA_PAGO Y NO CON LA FECHA_LIQUIDA

      LET v_preliq_f_liquida = TODAY;

--      LET v_preliq_f_liquida = v_pag_det_apvol_f_pago;

      --se invoca a función que calcula el monto en acciones       
      LET v_preliq_monto_acciones = fn_consulta_precio_fondo (v_pag_det_apvol_imp_ap_vol ,
                                                              v_pag_det_apvol_f_pago  ,
                                                              v_preliq_fondo_inversion ) ;
      --Se valida si no encontro el fondo se asiga 0  
      IF( v_preliq_monto_acciones IS NULL )THEN 
         LET v_preliq_monto_acciones	= 0 ;
      END IF 

      INSERT INTO pag_preliquida
                  ( f_liquida              ,
                    id_derechohabiente     ,
                    subcuenta              ,
                    fondo_inversion        ,
                    movimiento             ,
                    folio_liquida          ,
                    id_referencia          ,
                    monto_acciones         ,
                    monto_pesos            ,
                    f_valor                ,
                    f_registro             ,
                    h_registro             ,
                    origen)
                          
          VALUES  ( v_preliq_f_liquida                 ,
                    v_pag_det_apvol_id_derechohabiente ,
                    v_preliq_subcuenta                 ,
                    v_preliq_fondo_inversion           ,
                    v_preliq_movimiento                ,
                    p_folio                            ,
                    v_pag_det_apvol_id_referencia      ,
                    v_preliq_monto_acciones            ,
                    v_pag_det_apvol_imp_ap_vol         ,
                    v_pag_det_apvol_f_pago             ,
                    TODAY                              ,
                    v_preliq_h_registro                ,
                    "VOLUNTARIAS");
   END FOREACH

   -- el folio se actualiza a estatus de preliquidado
   UPDATE glo_folio
     SET status = 1
   WHERE folio = p_folio;
   
   -- el folio se actualiza a estatus de preliquidado
   UPDATE glo_ctr_archivo
      SET estado = 2
   WHERE folio = p_folio;
   
   UPDATE STATISTICS FOR TABLE pag_preliquida;

   -- si no hubo error, se indica que el proceso termino correctamente
   LET v_error_sql     = 0;
   LET v_isam_error    = 0;
   LET v_mensaje_error = "El proceso de preliquidación de aportaciónes voluntarias terminó correctamente";

   RETURN v_error_sql, 
          v_isam_error, 
          v_mensaje_error;

END PROCEDURE;


