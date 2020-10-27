






CREATE PROCEDURE "safreviv".fn_hps_preliquidar_traspaso_servicios(p_folio        DECIMAL(9,0) ,
                                                       p_usuario      CHAR(20))
RETURNING SMALLINT, INTEGER, VARCHAR(250);

-- registro de hps_det_aplica_servicio 

DEFINE v_das_id_derechohabiente          DECIMAL(9,0);
DEFINE v_das_id_det_aplica_servicio      DECIMAL(9,0);
DEFINE v_das_desc_mandato                CHAR(20);
DEFINE v_das_subcuenta                   SMALLINT; 
DEFINE v_das_movimiento                  SMALLINT; 
DEFINE v_das_monto_acciones              DECIMAL(22,2);
DEFINE v_das_monto_pesos                 DECIMAL(22,2);

-- regisro de hps_preliquida

DEFINE v_pr_f_liquida                        DATE; 
DEFINE v_pr_id_derechohabiente               DECIMAL(9,0);
DEFINE v_pr_subcuenta                        SMALLINT ;
DEFINE v_pr_fondo_inversion                  SMALLINT ;
DEFINE v_pr_movimiento                       SMALLINT ;
DEFINE v_pr_folio_liquida                    DECIMAL(9,0); 
DEFINE v_pr_id_referencia                    DECIMAL(9,0) ;
DEFINE v_pr_monto_acciones                   DECIMAL(22,2);
DEFINE v_pr_monto_pesos                      DECIMAL(22,2);
DEFINE v_pr_f_valor                          DATE;
DEFINE v_pr_f_registro                       DATE;
DEFINE v_pr_h_registro                       CHAR(08);
DEFINE v_pr_origen                           CHAR(20);


-- control de excepciones

DEFINE sql_err         INTEGER;
DEFINE isam_err        INTEGER;
DEFINE err_txt         CHAR(200);
DEFINE v_c_msj         VARCHAR(250);
DEFINE v_si_resultado  SMALLINT;


   -- se asigna el codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   --Se habilita el LOG del SP
   SET DEBUG FILE TO '/safreviv_int/BD/sp_hps_aplica_fondo_servicio.trace';
   TRACE ON;
   --TRACE 'Inicia el store procedure de preliquidacion de mandatos';

   --TRACE "Parte 1 - verifica folio en glo_folio ";
   -- se asume que no presenta error

   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET v_c_msj        = 'La preliquidacion de Traspaso de Fondos Servicios FINALIZO CORRECTAMENTE';


-- se actualizan estados de control de folio y control de aplicación a preliquidado 
-- para facilitar el reverso de la operacion

     UPDATE glo_folio 
     SET    status      = 1                  -- preliquidado
     WHERE  folio       = p_folio;

     UPDATE hps_ctr_aplica_servicio 
     SET    estado                = 100      -- preliquidado
     WHERE  folio_aplica_servicio = p_folio; 


     FOREACH SELECT a.id_derechohabiente        ,
                    a.id_det_aplica_servicio   ,
                    b.desc_mandato             ,
                    a.subcuenta                ,
                    a.movimiento               ,
                    a.mto_acciones           ,
                    a.mto_pesos
             INTO   v_das_id_derechohabiente      ,
                    v_das_id_det_aplica_servicio  ,
                    v_das_desc_mandato            , 
                    v_das_subcuenta               ,
                    v_das_movimiento              ,
                    v_das_monto_acciones          ,
                    v_das_monto_pesos 
             FROM   hps_det_aplica_servicio   a ,
                    mdt_cat_mandato           b ,
                    hps_ctr_aplica_servicio   c
             WHERE  c.folio_aplica_servicio   = p_folio
               AND  c.id_ctr_aplica_servicio  = a.id_ctr_aplica_servicio
               AND  a.id_cat_mandato          = b.id_cat_mandato
               AND  a.estado                  = 50  -- registrado


               LET v_pr_f_liquida            = TODAY                        ; 
               LET v_pr_id_derechohabiente   = v_das_id_derechohabiente     ;

               IF v_das_subcuenta = 51 THEN -- predial 
                  LET v_pr_movimiento = 312;
               ELIF v_das_subcuenta = 53 THEN 
                  LET v_pr_movimiento = 332;
               END IF 

               LET v_pr_fondo_inversion      = 11                           ; -- en aivs 
               LET v_pr_subcuenta            = 4                            ;
               LET v_pr_folio_liquida        = p_folio                      ; 
               LET v_pr_id_referencia        = v_das_id_det_aplica_servicio ;
               LET v_pr_monto_acciones       = - fn_consulta_precio_fondo(v_das_monto_pesos,today,11); 
               LET v_pr_monto_pesos          = - v_das_monto_pesos;
               LET v_pr_f_valor              = TODAY; 
               LET v_pr_f_registro           = TODAY;
               LET v_pr_h_registro           = CURRENT HOUR TO SECOND;
               LET v_pr_origen               = v_das_desc_mandato; 


               --- inserta cargo a la subcuenta 4 viv97

               INSERT INTO hps_preliquida VALUES (v_pr_f_liquida          ,
                                                  v_pr_id_derechohabiente ,
                                                  v_pr_subcuenta          ,  
                                                  v_pr_fondo_inversion    , 
                                                  v_pr_movimiento         ,
                                                  v_pr_folio_liquida      ,
                                                  v_pr_id_referencia      ,
                                                  v_pr_monto_acciones     ,
                                                  v_pr_monto_pesos        ,
                                                  v_pr_f_valor            ,
                                                  v_pr_f_registro         ,
                                                  v_pr_h_registro         ,
                                                  v_pr_origen              );       
                                        
               --- inserta abono  en subcuentas de mandatos predial y cuota conservacion

               LET v_pr_subcuenta      = v_das_subcuenta                             ;
               LET v_pr_movimiento     = v_das_movimiento                            ;
               LET v_pr_monto_acciones = fn_consulta_precio_fondo(v_das_monto_pesos,today,11) ;
               LET v_pr_monto_pesos    = v_das_monto_pesos                           ;

               INSERT INTO hps_preliquida VALUES (v_pr_f_liquida          ,
                                                  v_pr_id_derechohabiente ,
                                                  v_pr_subcuenta          ,  
                                                  v_pr_fondo_inversion    , 
                                                  v_pr_movimiento         ,
                                                  v_pr_folio_liquida      ,
                                                  v_pr_id_referencia      ,
                                                  v_pr_monto_acciones     ,
                                                  v_pr_monto_pesos        ,
                                                  v_pr_f_valor            ,
                                                  v_pr_f_registro         ,
                                                  v_pr_h_registro         ,
                                                  v_pr_origen              );       




     END FOREACH;

RETURN v_si_resultado, isam_err, v_c_msj;

END PROCEDURE;


