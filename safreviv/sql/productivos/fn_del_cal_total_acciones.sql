






CREATE FUNCTION "safreviv".fn_del_cal_total_acciones(
                p_fecha_valuacion CHAR(10),
                p_numero_acciones DECIMAL(18,6),
                p_fondo           SMALLINT)
  RETURNING SMALLINT, DECIMAL(18,6)

   DEFINE v_precio_fondo            DECIMAL(19,14);
   DEFINE v_total_acciones_valuadas DECIMAL(18,6);
   DEFINE v_estado_proceso          SMALLINT;

   LET v_total_acciones_valuadas = 0;
   -- Inicia Estatus Correcto
   LET v_estado_proceso = 0;
   -- Inicializa precio fondo
   LET v_precio_fondo = 0;

   -- Obtener el valor de la accion al dia actual
   SELECT precio_fondo
     INTO v_precio_fondo
     FROM glo_valor_fondo
    WHERE fondo = p_fondo
      AND f_valuacion = p_fecha_valuacion; -- formato MM/DD/YYYY

   IF(v_precio_fondo <=0)THEN
      -- Indica que NO encontro el valor de la accion
      LET v_estado_proceso = 1;
      RETURN v_estado_proceso, v_total_acciones_valuadas;
   END IF

   -- Realiza el calculo del total de acciones
   LET v_total_acciones_valuadas = v_precio_fondo * p_numero_acciones;
   -- Automaticamente se redondea al deciml superior

 RETURN v_estado_proceso, v_total_acciones_valuadas;
END FUNCTION;


