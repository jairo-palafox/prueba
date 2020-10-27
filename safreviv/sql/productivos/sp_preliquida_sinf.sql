






CREATE PROCEDURE "safreviv".sp_preliquida_sinf(p_folio DECIMAL(9,0), p_usuario CHAR(20))
RETURNING SMALLINT, SMALLINT, VARCHAR(255)

   --pag_cza_soloifv
   DEFINE v_cza_folio                    DECIMAL(9,0);
   DEFINE v_cza_nrp                      CHAR(11);
   DEFINE v_cza_folio_sua                CHAR(6);
   DEFINE v_cza_f_valor_4seguros         DATE;
   --pag_det_trab_sifv                   
   DEFINE v_folio                        DECIMAL(9,0) ;    --folio
   DEFINE v_id_referencia                DECIMAL(9,0) ;    --id_referencia
   DEFINE v_id_derechohabiente           DECIMAL(9,0) ;    --id_derechohabiente
   DEFINE v_nrp                          CHAR(11)     ;    --nrp
   DEFINE v_periodo_pago                 CHAR(6)      ;    --periodo_pago
   DEFINE v_folio_sua                    DECIMAL(6,0) ;    --folio_sua
   DEFINE v_sdi                          DECIMAL(10,2);    --sdi
   DEFINE v_imp_ap_pat                   DECIMAL(12,2);    --imp_ap_pat
   DEFINE v_cve_concepto                 SMALLINT     ;    --cve_concepto
   DEFINE v_f_pago                       DATE         ;    --f_pago
   DEFINE v_f_proceso                    DATE         ;    --f_proceso
   DEFINE v_num_crd_trab                 DECIMAL(10,0);    --num_crd_trab
   DEFINE v_cve_ent_recauda              CHAR(3)      ;    --cve_ent_recauda
   DEFINE v_imp_am_crd                   DECIMAL(12,2);    --imp_am_crdç            
   DEFINE v_pag_sif_id_referencia        DECIMAL(9,0) ;
   DEFINE v_pag_sif_cve_ent_receptora    CHAR(3)      ;
   DEFINE v_pag_sif_nrp                  CHAR(11)     ;
   DEFINE v_pag_sif_f_pago               DATE         ;
   DEFINE v_pag_sif_id_derechohabiente   DECIMAL(9,0) ;
   DEFINE v_pag_sif_localiza_trabajador  CHAR(1)      ;
   DEFINE v_pag_sif_imp_ap_pat           DECIMAL(18,2);
   DEFINE v_pag_sif_imp_am_cre           DECIMAL(18,2);
   DEFINE v_pag_sif_aiv_ap_pat           DECIMAL(18,2);
   --pag_preliquida
   DEFINE v_preliq_f_liquida             DATE;                     --f_liquida
   DEFINE v_preliq_id_derechohabiente    DECIMAL(9,0);             --id_derechohabiente
   DEFINE v_preliq_subcuenta             SMALLINT;                 --subcuenta
   DEFINE v_preliq_fondo_inversion       SMALLINT;                 --fondo_inversion
   DEFINE v_preliq_movimiento            SMALLINT;                 --movimiento
   DEFINE v_preliq_folio_liquida         DECIMAL(9,0);             --folio_liquida
   DEFINE v_preliq_id_referencia         DECIMAL(9,0);             --id_referencia
   DEFINE v_preliq_monto_acciones        DECIMAL(18,2);            --monto_acciones
   DEFINE v_preliq_monto_pesos           DECIMAL(18,2);            --monto_pesos
   DEFINE v_preliq_f_valor               DATE;                     --f_valor
   DEFINE v_preliq_f_registro            DATE;                     --f_registro
   DEFINE v_preliq_h_registro            DATETIME HOUR TO SECOND;  --h_registro
   DEFINE v_preliq_usuario               CHAR(20);                 --usuario
   -- Control de Excepciones
   DEFINE sql_err                        SMALLINT;
   DEFINE isam_err                       SMALLINT;
   DEFINE err_txt                        VARCHAR(255);
   DEFINE v_si_resultado                 SMALLINT;

  --manejo de excepciones
  ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
  END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_preliquida_sinf.trace';
   --TRACE 'Inicia sp_preliquida_sinf con Folio:' || p_folio;
   
   --Inicia con la lectura de todos los registros de la tabla pag_det_trab_sifv
   --por cada derechohabiente
   
   --TRACE 'Inicia FOREACH :' ;
   FOREACH SELECT id_referencia            ,
                  nrp                      ,
                  f_pago                   ,
                  id_derechohabiente       ,
                  imp_ap_pat               ,
                  imp_am_cre               ,
                  aiv_ap_pat               
             INTO v_pag_sif_id_referencia         ,                  
                  v_pag_sif_nrp                   ,
                  v_pag_sif_f_pago                ,
                  v_pag_sif_id_derechohabiente    ,
                  v_pag_sif_imp_ap_pat            ,
                  v_pag_sif_imp_am_cre            ,
                  v_pag_sif_aiv_ap_pat             
            FROM cta_his_pagos
           WHERE folio = p_folio
      
      --modifica Rubén Haro Castro 
      --21 de JUnio de 2012
      --no es necesario validar nrp ya es que solo infonavit 
      {
      IF (v_nrp[1,2] = "99") THEN
         LET v_bandera_nrp = 1;
      ELSE
         LET v_bandera_nrp = 0;
      END IF
      }
      --Constantes para todos los derechohabintes que pertenecen al folio_sua
      LET v_preliq_fondo_inversion = 11;
      LET v_preliq_folio_liquida   = p_folio;
      LET v_preliq_f_valor         = v_pag_sif_f_pago;
      LET v_preliq_id_referencia   = v_pag_sif_id_referencia;

      --#### aqui inicia el proceso de preliquidación afectando a 4 tablas
      --TRACE ('v_pag_sif_aiv_ap_pat'||v_pag_sif_aiv_ap_pat);
      --Primer registro 1 Para la Subcuenta 43 y mov 81
      LET v_preliq_f_liquida          = TODAY;
      LET v_preliq_id_derechohabiente = v_pag_sif_id_derechohabiente;
       --modifica Rubén Haro Castro 
      --21 de JUnio de 2012
      --no es necesario validar nrp ya es que solo infonavit 
      {
      IF v_bandera_nrp = 1 THEN
        LET v_preliq_subcuenta          = 44;
      ELSE
        LET v_preliq_subcuenta          = 43;
        -- HCRG 16-04-2012 para subcuenta 43 y movimiento 81, siefore = 0
        LET v_preliq_fondo_inversion = 0;
      END IF
      }
      LET v_preliq_movimiento         = 81;
      LET v_preliq_monto_pesos        = v_pag_sif_imp_ap_pat;
      LET v_preliq_monto_acciones     = v_pag_sif_aiv_ap_pat;
      LET v_preliq_f_registro         = TODAY; --en caso de realizar la preliquidacion con muchos registros, actualiza cada registro
      LET v_preliq_h_registro         = CURRENT HOUR TO SECOND; --actualiza tiempo para cada registro

      --Solo se inserta si el registro tiene importe mayo a CERO
      IF v_preliq_monto_pesos > 0 THEN
      	 -- se asigan ala subcuenta  correspondiente 
         LET v_preliq_subcuenta          = 44;
      		--modifica Rubén Haro Castro 
        --21 de JUnio de 2012
        --no es necesario calcular aivs el archivo cde carga contiene ese dato 
      	--se agrega el día 18 de junio para el calculo de aivs
      	{
      	IF v_preliq_subcuenta  = 44  AND   v_preliq_movimiento = 81 THEN       		      		      
      		 --se calcual el mon to de acciones 
      	   LET v_preliq_monto_acciones = fn_consulta_precio_fondo (v_preliq_monto_pesos ,
      	                                                         TODAY ) ;
      	END IF
      	}
        --se insertan los 
         INSERT INTO pag_preliquida
                        ( 
                          f_liquida              ,                  
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
                          origen                                    
                        )                                           
                VALUES  (                                           
                          v_preliq_f_liquida            ,           
                          v_preliq_id_derechohabiente   ,           
                          v_preliq_subcuenta            ,           
                          v_preliq_fondo_inversion      ,           
                          v_preliq_movimiento           ,           
                          v_preliq_folio_liquida        ,           
                          v_preliq_id_referencia        ,           
                          v_preliq_monto_acciones       ,           
                          v_preliq_monto_pesos          ,           
                          v_preliq_f_valor              ,           
                          v_preliq_f_registro           ,
                          v_preliq_h_registro           ,
                          v_pag_sif_nrp
                        );
      END IF

      --segundo registro
      -- HCRG 16-04-2012 si subcuenta fué 43 y movimiento 81, siefore regresa a 11, se corrige si v_pag_sif_imp_am_cre      
      -- el fondo de inversion es 0 
      LET v_preliq_fondo_inversion = 10;
      LET v_preliq_f_liquida      = TODAY;
      
      --modifica Rubén Haro Castro 
      --21 de JUnio de 2012
      --no es necesario validar nrp ya es que solo infonavit 
      {
      IF v_bandera_nrp = 1 THEN
        LET v_preliq_subcuenta          = 44;
      ELSE
        LET v_preliq_subcuenta      = 41;
      END IF
      }
      LET v_preliq_movimiento     = 81;
      LET v_preliq_monto_pesos    = v_pag_sif_imp_am_cre;
      LET v_preliq_monto_acciones = v_pag_sif_imp_am_cre;
      LET v_preliq_f_registro     = TODAY;
      LET v_preliq_h_registro     = CURRENT HOUR TO SECOND;

      --Solo se inserta si el registro tiene importe mayo a CERO
      IF v_preliq_monto_pesos > 0 THEN
      	 -- se asigan ala subcuenta  correspondiente 
         LET v_preliq_subcuenta          = 43;                              
      	
      	--modifica Rubén Haro Castro 
        --21 de JUnio de 2012
        --no es necesario calcular aivs el archivo cde carga contiene ese dato 
      	{
         --se agrega el día 18 de junio para el calculo de aivs
      	IF (v_preliq_subcuenta  = 44)  AND (v_preliq_movimiento = 81) THEN
      		 --se calcual el mon to de acciones 
      	   LET v_preliq_monto_acciones = fn_consulta_precio_fondo (v_preliq_monto_pesos ,
      	                                                         TODAY ) ;
      	END IF
      	}
        --se insertan los datos en la tabla de preliquidación 
         INSERT INTO pag_preliquida
                       ( f_liquida, 
                         id_derechohabiente, 
                         subcuenta, 
                         fondo_inversion, 
                         movimiento,
                         folio_liquida, 
                         id_referencia, 
                         monto_acciones, 
                         monto_pesos, 
                         f_valor,
                         f_registro, 
                         h_registro, 
                         origen
                       )
                 VALUES( v_preliq_f_liquida,
                         v_preliq_id_derechohabiente,
                         v_preliq_subcuenta,
                         v_preliq_fondo_inversion,
                         v_preliq_movimiento,
                         v_preliq_folio_liquida,
                         v_preliq_id_referencia,
                         v_preliq_monto_acciones,
                         v_preliq_monto_pesos,
                         v_preliq_f_valor,
                         v_preliq_f_registro,
                         v_preliq_h_registro,
                         v_pag_sif_nrp
                        );
      END IF
   END FOREACH

   -- el folio se actualiza a estatus de preliquidado
   UPDATE glo_folio
     SET status = 1
   WHERE folio = p_folio;

   {
   UPDATE safre_viv:glo_ctr_archivo
      SET estado = 3
    WHERE folio = p_folio;
   }
   -- si no hubo error, se indica que el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de preliquidación de SOLO INFONAVIT terminó correctamente";

   RETURN v_si_resultado, isam_err, err_txt;

END PROCEDURE;


