






CREATE FUNCTION "safreviv".fn_dpe_genera_pagos_parciales(p_usuario_cod CHAR(20)) 
   RETURNING INTEGER, INTEGER, CHAR(200)

 -- Tabla de registro de pagos parciales
 DEFINE v_parcial_id_dpe_referencia    DECIMAL(9,0) ;
 DEFINE v_parcial_folio                DECIMAL(9,0) ;
 DEFINE v_parcial_reg_patronal_imss    CHAR(11)     ;
 DEFINE v_parcial_periodo_pago         CHAR(6)      ;
 DEFINE v_parcial_imp_viv_dev          DECIMAL(16,6);
 DEFINE v_parcial_avis_viv_dev         DECIMAL(16,6);
 DEFINE v_parcial_diagnostico          SMALLINT     ;
 DEFINE v_parcial_resul_op             SMALLINT     ;
 DEFINE v_parcial_diag_procesa         SMALLINT     ;
 DEFINE v_parcial_folio_respuesta      DECIMAL(9,0) ;

-- detalle en safre_viv
 DEFINE v_dpe_id_dpe_referencia     DECIMAL(9,0) ;
 DEFINE v_dpe_folio                 DECIMAL(9,0) ;
 DEFINE v_dpe_id_derechohabiente    DECIMAL(9,0)   ;
 DEFINE v_dpe_nombre_trabajador     CHAR(50)     ;
 DEFINE v_dpe_total_pagar_avis_viv_dev  DECIMAL(16,6);
 DEFINE v_dpe_imp_viv_dev           DECIMAL(16,6);
 DEFINE v_dpe_reg_patronal_imss     CHAR(11);
 DEFINE v_dpe_periodo_pago          CHAR(6);
 
 DEFINE v_pagado_imp_viv_dev        DECIMAL(16,6);
 DEFINE v_total_pagado_avis_viv_dev DECIMAL(16,6);
 DEFINE v_d_saldo_disponible_aivs   DECIMAL(16,6);
 DEFINE v_d_saldo_calculado_aivs   DECIMAL(16,6);

 -- variables de soporte al proceso
 DEFINE v_b_exito                       SMALLINT; -- booleana para indicar si el proceso termino bien
 DEFINE v_id_derechohabiente            decimal(9); -- ID de derechohabiente asociado a un NSS
 DEFINE v_i_resultado                   INTEGER; -- resultado de la operacion
 -- subcuenta y
 DEFINE v_subcuenta_tmp                 SMALLINT; -- subcuenta temporal
 DEFINE v_fondo_inversion_tmp           SMALLINT; -- fondo temporal
 DEFINE v_movimiento_tmp                SMALLINT; -- clave de movimiento temporal
 DEFINE v_si_procesa_insercion          SMALLINT;
 --
 DEFINE v_i_registros_insertados        INTEGER;
 DEFINE v_i_total_registros_encontrados INTEGER;
 DEFINE v_dte_fecha_hoy                 DATE;
 DEFINE v_si_estado                     SMALLINT;
 
 DEFINE v_si_total_pagar                SMALLINT;
 DEFINE v_si_status_saldos              SMALLINT;
 
 -- Tipos status de pagos
 DEFINE c_pago_total_nss                SMALLINT;
 DEFINE c_pago_parcial_nss              SMALLINT;
 DEFINE c_pago_por_preliquidar_total    SMALLINT;
 DEFINE c_pago_por_preliquidar_parcial  SMALLINT;
 DEFINE c_pago_preliquidado_total       SMALLINT;
 DEFINE c_pago_preliquidado_parcial     SMALLINT;
 DEFINE c_pago_liquidado_total          SMALLINT;
 DEFINE c_pago_liquidado_parcial        SMALLINT;
 DEFINE c_pago_enviado_procesar_total   SMALLINT;
 DEFINE c_pago_enviado_procesar_parcial SMALLINT;
 
 DEFINE v_c_cadena       CHAR(200);
 DEFINE  v_tmp_nss       CHAR(11);
 DEFINE  v_resultado     SMALLINT;
 DEFINE  v_saldo_pesos   DECIMAL(16,6) ;
 

 ON EXCEPTION IN (-206)
   LET v_i_resultado = -206;
   LET v_si_procesa_insercion = 0;
   LET v_i_registros_insertados = 0;
   LET v_c_cadena = 'error:';
   RETURN v_i_resultado, v_i_registros_insertados, v_c_cadena;
 END EXCEPTION
 
 --SET DEBUG FILE TO 'trace.dpe.preliquida.16032012.txt';
 
 -- Constantes de estatus de diagnostico y de pagos
 LET c_pago_total_nss                = 0;
 LET c_pago_parcial_nss              = 1;
 LET c_pago_por_preliquidar_total    = 2;
 LET c_pago_por_preliquidar_parcial  = 3;
 LET c_pago_preliquidado_total       = 4;
 LET c_pago_preliquidado_parcial     = 5;
 LET c_pago_liquidado_total          = 6;
 LET c_pago_liquidado_parcial        = 7;
 LET c_pago_enviado_procesar_total   = 8;
 LET c_pago_enviado_procesar_parcial = 9;

 
 

 -- se asume que el proceso termina correctamente
 LET v_i_resultado = 0;
 -- <Inicializa status de proceso de inserción>
 LET v_si_procesa_insercion = 0;
 -- <Inicializa total de registros a procesar en preliquidación>
 LET v_i_total_registros_encontrados = 0;
 -- <Inicializa total de registros insertados con importe mayor a cero>
 LET v_i_registros_insertados = 0;
 
 LET v_dte_fecha_hoy = TODAY;
 LET v_si_status_saldos = 0;

 -- se obtienen todos los registros de la tabla temporal
 LET v_c_cadena = "Fuera del cliclo";
 FOREACH --cur_tmp_detalle FOR
   SELECT
         a.id_dpe_referencia
        ,a.folio
        ,a.id_derechohabiente
        ,a.avis_viv_dev
        ,a.imp_viv_dev
        ,a.reg_patronal_imss
        ,a.periodo_pago
        ,a.nss
     INTO
         v_dpe_id_dpe_referencia
        ,v_dpe_folio
        ,v_dpe_id_derechohabiente
        ,v_dpe_total_pagar_avis_viv_dev
        ,v_dpe_imp_viv_dev
        ,v_dpe_reg_patronal_imss
        ,v_dpe_periodo_pago
        ,v_tmp_nss
   FROM dpe_sol_trabajador a
   WHERE a.estado_solicitud = 1  -- solo solicitudes aceptadas
     AND a.porcentaje_dev   = 1  -- indica que es un pago parcial
     AND a.diagnostico      = c_pago_parcial_nss -- indica que es un pago parcial
     
   LET v_i_total_registros_encontrados = 
       v_i_total_registros_encontrados + 1;
   

   -- < Por DEFAULT total pagar es parcial>
   LET v_si_total_pagar = 0;
   
   -- [ Contador de total de registros a procesar]
   
   -- <Iniciliza variables de totales>
   LET v_pagado_imp_viv_dev        = 0;
   LET v_total_pagado_avis_viv_dev = 0;
   LET v_d_saldo_calculado_aivs    = 0;
   
   
   LET v_c_cadena = "Obtiene saldo total";
   -- [Obtener el total de datos]
   EXECUTE FUNCTION fn_dpe_obtiene_saldo_nss(v_dpe_id_dpe_referencia, v_dpe_folio, 
                v_dpe_reg_patronal_imss, v_dpe_periodo_pago)
      INTO v_pagado_imp_viv_dev, v_total_pagado_avis_viv_dev, v_si_status_saldos, v_c_cadena;
   
   -- # [Evita realizar mas cargos a nss antes de liquidar pago anterior.]
   IF(v_si_status_saldos <> 0)THEN
      -- Indica que NO se puede generar nuevo pago parcial para el registro
      --  ya que cuenta con saldo pendiente de liquidar.
      LET v_c_cadena = "Se brinco y termino por status_saldos <> 0";
      CONTINUE FOREACH;
   END IF
   
   -- [Verificar total_aivs pagado es menor al total aivs a pagar]
   IF(v_total_pagado_avis_viv_dev < v_dpe_total_pagar_avis_viv_dev)THEN
      -- 
      -- Indica que aun hay aivs por pagar del total.
      LET v_c_cadena = "Indica que aun hay aivs por pagar del total";
      
      -- # [Obtener el saldo disponible del trabajador.]
      LET v_d_saldo_disponible_aivs = 0;
      -- # [Subcuenta  VIVIENDA 97]
      LET v_subcuenta_tmp       =  4;
      EXECUTE FUNCTION fn_saldo_dia( v_tmp_nss
                         ,v_dpe_id_derechohabiente
                         ,v_subcuenta_tmp
                         ,NULL -- fecha saldo en nulo indica la fecha actual
                       )
         INTO v_resultado, v_d_saldo_disponible_aivs, v_saldo_pesos;
      IF(v_resultado <> 0)THEN
         -- No hay saldo disponible para el nss
         LET v_d_saldo_disponible_aivs = 0;
      END IF
      
      IF(v_d_saldo_disponible_aivs >0)THEN
         -- #[Si existe saldo disponible, por lo que se genera un cargo]
         LET v_c_cadena = "Si existe saldo disponible, por lo que se genera un cargo";
         
         LET v_d_saldo_calculado_aivs = 
             v_dpe_total_pagar_avis_viv_dev - v_total_pagado_avis_viv_dev;
         
         IF(v_d_saldo_disponible_aivs < v_d_saldo_calculado_aivs)THEN
            -- #[Se calcula el pacial que se puede cubrir]
            -- #[En este caso el pago parcial es slado disponible]
            LET v_d_saldo_calculado_aivs = v_d_saldo_disponible_aivs;
            LET v_c_cadena = "Se calcula el saldo para un parcial";
         ELSE
            -- Indica que se puede cubrir el total faltante
            -- #[ Pago complementario o final]
            -- #[ Equivale al saldo calculado]
            
            -- Bandera para indicar que se libera este proceso
            LET v_si_total_pagar = 1;
            LET v_c_cadena = "Pago complementario o final";
            
         END IF
         
         LET v_c_cadena = "convierte aivs en pesos";
         -- #[ Generar importe correspondiente en pesos]
         EXECUTE FUNCTION fn_del_cal_total_acciones(
                   v_dte_fecha_hoy, 
                   v_d_saldo_calculado_aivs,
                   11)
            INTO v_si_estado, v_pagado_imp_viv_dev;
      
         IF(v_si_estado <> 0)THEN
            -- #[Imposible continuar, no existe dato registro ]
            -- #[ VERIFICAR QUE REACCION SE TOMA PARA RECHAZAR]
            -- #[Imposible continuar, no existe dato registro ]
            
            LET v_c_cadena = "error al generar aivs a pesos";
           
            -- #[Por lo pronto se genera un rechazo]
            EXECUTE PROCEDURE sp_dpe_inserta_rechazo( v_dpe_folio, 10 , 
                         v_dpe_id_dpe_referencia, 'NO' , 22, 
                         v_dte_fecha_hoy||' aivs:'||v_d_saldo_calculado_aivs);
            
            RETURN v_i_resultado, v_i_total_registros_encontrados, v_c_cadena;
            --CONTINUE FOREACH;
         END IF
         
         -- Asignar datos para insercion
         LET v_parcial_id_dpe_referencia = v_dpe_id_dpe_referencia;
         LET v_parcial_folio             = v_dpe_folio;
         LET v_parcial_reg_patronal_imss = v_dpe_reg_patronal_imss;
         LET v_parcial_periodo_pago      = v_dpe_periodo_pago;
         LET v_parcial_imp_viv_dev       = v_pagado_imp_viv_dev;
         LET v_parcial_avis_viv_dev      = v_d_saldo_calculado_aivs;
         -- Se pone en STATUS de pago pendiente de liquidar
         LET v_parcial_diagnostico       = c_pago_por_preliquidar_parcial;

         -- Los alimenta procesar
         LET v_parcial_resul_op          = 0;
         LET v_parcial_diag_procesa      = 0;
         LET v_parcial_folio_respuesta   = 0;

         LET v_c_cadena = "Antes de insertar registro de paciales";
         -- Insertar registro de pago parcal generado.
         INSERT INTO dpe_sol_trab_parcial
         (
            id_dpe_referencia
           ,folio            
           ,reg_patronal_imss
           ,periodo_pago     
           ,imp_viv_dev      
           ,avis_viv_dev     
           ,diagnostico      
           ,resul_op         
           ,diag_procesa     
           ,folio_respuesta  
         )
         VALUES 
         (
            seq_dpe_sol_trab_parcial.NEXTVAL --v_parcial_id_dpe_referencia
           ,v_parcial_folio            
           ,v_parcial_reg_patronal_imss
           ,v_parcial_periodo_pago     
           ,v_parcial_imp_viv_dev      
           ,v_parcial_avis_viv_dev     
           ,v_parcial_diagnostico      
           ,v_parcial_resul_op         
           ,v_parcial_diag_procesa     
           ,v_parcial_folio_respuesta  
         );
         
         
         -- [Actualiza total de registros insertados mayor a cero]
         LET v_i_registros_insertados = 
             v_i_registros_insertados + 1;
         
         -- En caso de estar cubierto el pago total de acciones
         IF(v_si_total_pagar = 1)THEN
            
            LET v_c_cadena = "En caso de estar cubierto el pago total de acciones";
            -- Liberar el pago continuo para este proceso
            --   Indica pagado completamente
            UPDATE dpe_sol_trabajador
               SET porcentaje_dev = 100 
             WHERE estado_solicitud = 1
               AND porcentaje_dev   = 1
               AND diagnostico      = c_pago_parcial_nss
               AND id_dpe_referencia= v_dpe_id_dpe_referencia
               AND folio            = v_dpe_folio
               AND reg_patronal_imss= v_dpe_reg_patronal_imss
               AND periodo_pago     = v_dpe_periodo_pago;
               
         END IF


      ELSE
         -- #[No existe saldo disponible por lo que no se liquida]
         -- #[  ningun parcial para este ciclo.                  ]
         LET v_c_cadena = "No existe saldo disponible por lo que no se liquida";
         CONTINUE FOREACH;
      END IF
      
      
      
   ELSE
      -- # Esto no debe ocurrir jajajajajaja
      -- # Error de integridad, este punto se trata en otro paso
      -- # El registro de de_sol_trabajador, debe estar en estado pagado total
      -- #  dentro del porcentaje
      -- ##
      -- ##      -- Liberar el pago continuo para este proceso
      -- ##      --   Indica pagado completamente
      -- ##      UPDATE dpe_sol_trabajador
      -- ##         SET porcentaje_dev = 100 
      -- ##       WHERE estado_solicitud = 1
      -- ##         AND porcentaje_dev   = 1
      -- ##         AND diagnostico      = 1
      -- ##         AND id_dpe_referencia= v_dpe_id_dpe_referencia
      -- ##         AND folio            = v_dpe_folio
      -- ##         AND reg_patronal_imss= v_dpe_reg_patronal_imss
      -- ##         AND periodo_pago     = v_dpe_periodo_pago;
      -- ##
      LET v_c_cadena = "Esto no debe ocurrir jajajajajaja";

      CONTINUE FOREACH;
   END IF
   

 END FOREACH;
 
 
   LET v_c_cadena = "termina completo foreach";
 
   RETURN v_i_resultado, v_i_registros_insertados, v_c_cadena; 
END FUNCTION;


