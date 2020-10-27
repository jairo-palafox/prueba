-----------------------------------------------------------------------------------------
-- Modulo        => PAG                                                                    
-- Programa      => PAGW90
-- Objetivo      => Función que ingresa información de pagos CambiaVit
-- Autor         => GERARDO ALFONSO VEGA PAREDES                                           
-- Fecha inicio  => 21 de Agosto de 2018
-- Requerimiento => 
-----------------------------------------------------------------------------------------
-- Modificación => Cambiar tamaño del cambpo marca_cambio_casa de 2 a 4
-- Fehca        => 3 de Diciembre de 2018.
-- Autor        => GERARDO ALFONSO VEGA PAREDES.
-- Clave cambio => sisxviii-16
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod

GLOBALS

   DEFINE 
      p_nss                CHAR(11),
      p_curp               CHAR(18),
      p_credito_infonavit  DECIMAL(10,0),
      p_cod_identificacion CHAR(10),
      p_marca_cambio_casa  CHAR(4),   --sisxviii-16
      p_numero_caso        DECIMAL(10,0),
      p_f_pago             DATE,
      p_monto_deposito     DECIMAL(15,2),
      v_result_operacion   CHAR(02),
      v_rechazo            SMALLINT,
      p_pid                DECIMAL(9,0)

END GLOBALS

MAIN
   
   LET p_nss                = ARG_VAL(1)
   LET p_curp               = ARG_VAL(2)
   LET p_credito_infonavit  = ARG_VAL(3)
   LET p_cod_identificacion = ARG_VAL(4)
   LET p_marca_cambio_casa  = ARG_VAL(5)
   LET p_numero_caso        = ARG_VAL(6)
   LET p_f_pago             = ARG_VAL(7)
   LET p_monto_deposito     = ARG_VAL(8)  
  
   CALL fn_carga_pago()
--   DISPLAY "v_result_operacion, rechazo y nss: ",v_result_operacion," ",v_rechazo," ",p_nss
   RETURN v_result_operacion,v_rechazo,p_nss

END MAIN

FUNCTION fn_carga_pago()
   
   DEFINE 
      v_folio            DECIMAL(9,0),
      v_usuario_cod      CHAR(20),
      v_id_derecho       DECIMAL(9,0),
      v_cza_f_proceso    DATETIME YEAR TO SECOND,
      v_f_registro       DATE,
      r_resultado_opera  SMALLINT,
      r_b_valida         SMALLINT,
      v_estatus          SMALLINT,
      p_mensaje          STRING,
      p_titulo           STRING

   CALL fn_display_proceso(0,"INTEGRACIÓN")

   -- Genera Folio
   LET v_folio = 0
   
   SELECT usuario
   INTO   v_usuario_cod
   FROM   seg_modulo
   WHERE  modulo_cod = "pag";
      
   -- se genera el pid para el proceso
   CALL fn_genera_pid(g_proceso_reg_pag_svt,g_opera_cod_pag_integracion,v_usuario_cod) RETURNING p_pid

   CALL fn_inicializa_proceso(p_pid,g_proceso_reg_pag_svt,g_opera_cod_pag_integracion,v_folio,
                              "PAGW90","",v_usuario_cod)
                     RETURNING r_resultado_opera   


   -- Inicia operacion de integración.
   DISPLAY "PID: ", p_pid
   DISPLAY "PROCESO COD: ", g_proceso_reg_pag_svt
   DISPLAY "Opera COD: ", g_opera_cod_pag_integracion
   DISPLAY "FOLIO: ", v_folio
   DISPLAY "PROGRAMA: PAGW90" 
   DISPLAY "USUARIO: ", v_usuario_cod
   
   -- Se registra el inicio de la operacion
   CALL fn_actualiza_opera_ini(p_pid,g_proceso_reg_pag_svt,g_opera_cod_pag_integracion,v_folio,
                  "PAGW90","",v_usuario_cod) RETURNING r_b_valida 

   DISPLAY "Valida: ", r_b_valida
   -- Finaliza iniciación de integración.

   LET v_cza_f_proceso  = CURRENT;
   LET v_f_registro = TODAY;

   LET v_id_derecho = NULL;

   SELECT FIRST 1 id_derechohabiente
   INTO   v_id_derecho
   FROM   afi_derechohabiente
   WHERE  nss = p_nss;

   IF v_id_derecho IS NULL OR v_rechazo <> 0 THEN
      LET v_id_derecho = 0 ;
      LET v_result_operacion    = "02";  -- codigo de rechazo
   ELSE
      LET v_result_operacion    = "01";  -- codigo de aceptado
   END IF

   LET v_rechazo = 0

   IF p_credito_infonavit IS NULL OR p_credito_infonavit = 0 THEN
      LET v_rechazo = 3
   END IF
   IF p_f_pago IS NULL THEN
      LET v_rechazo = 7
   END IF
   IF p_monto_deposito = 0 OR p_monto_deposito IS NULL THEN
      LET v_rechazo = 8
   END IF
   
   INSERT INTO pag_det_cvt(
      folio,
      id_referencia,
      id_derechohabiente,
      curp,
      credito_infonavit,
      cod_identificacion,
      marca_cambio_casa,
      num_caso,
      f_pago,
      monto_deposito,
      f_registro,
      ind_registro,
      result_operacion)
   VALUES (
      v_folio,
      1,
      v_id_derecho,
      p_curp,
      p_credito_infonavit,
      p_cod_identificacion,
      p_marca_cambio_casa,
      p_numero_caso,
      p_f_pago,
      p_monto_deposito,
      v_f_registro,
      v_rechazo,
      v_result_operacion);

   -- Se agrega sentencia de update statics a las tablas temporales
   UPDATE STATISTICS FOR TABLE pag_det_cvt;

   --Se registra el FIN DE LA OPERACION COMO EXITOSA
   CALL fn_actualiza_opera_fin(p_pid,
                               g_proceso_reg_pag_svt,
                               g_opera_cod_pag_integracion)
                               RETURNING v_estatus
                              
   LET p_mensaje = "Integración realizada con éxito.\nYa se puede continuar con la Preliquidación."
   LET p_titulo = "Finalización de operación - CambiaVit - Integración"
   
   CALL fn_correo_proceso(p_pid, g_proceso_reg_pag_svt, 
                          g_opera_cod_pag_integracion, 
                          NULL, p_titulo,p_mensaje)

   CALL fn_display_proceso(1,"INTEGRACIÓN")

END FUNCTION