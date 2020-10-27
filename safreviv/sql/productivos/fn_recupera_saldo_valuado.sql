






CREATE FUNCTION "safreviv".fn_recupera_saldo_valuado(v_nss                 CHAR(11),
                                          v_id_derechohabiente  DECIMAL(9,0),
                                          v_subcuenta           SMALLINT,
                                          v_fec_saldo           DATE,
                                          v_fondo               SMALLINT
                                          )
                           RETURNING DECIMAL(16,6),DECIMAL(16,6),SMALLINT

   DEFINE v_precio_fondo                DECIMAL(16,6);
   DEFINE r_saldo_acciones              DECIMAL(16,6);
   DEFINE r_total_saldo                 DECIMAL(16,6);
   DEFINE r_saldo_pesos                 DECIMAL(16,6);
   DEFINE v_resultado_consulta          SMALLINT     ;
   DEFINE v_s_sql                       varchar(200) ;
   DEFINE f_ultima_val                  DATE         ;
   DEFINE v_ctrl_val                    SMALLINT;

   LET v_precio_fondo                = 0    ;
   LET r_saldo_acciones              = 0    ;
   LET r_saldo_pesos                 = 0    ;
   LET v_resultado_consulta          = 0    ;
   LET r_total_saldo                 = 0    ;
   LET v_s_sql                       = ""   ;
   LET f_ultima_val                  = TODAY;
   LET v_precio_fondo                = 0    ;
   LET v_ctrl_val                    = 1    ;
   

   --se coloca la funcion saldo_dia para calcular el saldo del derechohabiente
   --conforme ala validacion de preliquidacion
   --Operacion aplica con calculo de la funcion
   EXECUTE FUNCTION fn_saldo_dia(v_nss,v_id_derechohabiente,v_subcuenta,v_fec_saldo)
                                      INTO v_resultado_consulta, r_saldo_acciones, r_saldo_pesos  ;

    --solucion temporal hasta que valide fecha de valuacion
   LET f_ultima_val = TODAY ;
   SELECT NVL(precio_fondo,0)
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE  f_valuacion = f_ultima_val
      AND fondo = v_fondo;

    --correccion por falta de datos
    IF v_precio_fondo <= 0 THEN
      SELECT max(f_valuacion)
        INTO f_ultima_val
        FROM glo_valor_fondo
       WHERE fondo = v_fondo;

      SELECT precio_fondo
        INTO v_precio_fondo
        FROM glo_valor_fondo
       WHERE f_valuacion = f_ultima_val
       AND fondo = v_fondo;
       
       LET v_ctrl_val = 0;
    END IF

   LET r_total_saldo = v_precio_fondo * r_saldo_acciones;

RETURN r_total_saldo,r_saldo_acciones, v_ctrl_val ;
END FUNCTION;


