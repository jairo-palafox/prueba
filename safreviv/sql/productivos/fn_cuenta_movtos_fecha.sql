






CREATE FUNCTION "safreviv".fn_cuenta_movtos_fecha(v_id_derechohabiente    DECIMAL(10,0),
                                       v_modulo                CHAR(3),
                                       v_subcuenta             SMALLINT,
                                       v_fecha                 DATE)
       RETURNING INTEGER

DEFINE v_movimientos     CHAR(100);
DEFINE v_tabla           CHAR(20);
DEFINE v_query           CHAR(5000);
DEFINE v_total_movtos    INTEGER;
DEFINE v_cuenta_movtos   INTEGER;
DEFINE v_fecha_hasta     DATE; 

   IF v_fecha IS NULL THEN
      LET v_fecha_hasta = TODAY;
   ELSE 
      LET v_fecha_hasta = v_fecha;
   END IF 
   
   IF v_subcuenta = 0 THEN 
      LET v_movimientos = '';
      
      IF v_modulo = 'ret' THEN 
         LET v_movimientos = '192, 212, 222, 822';
      END IF
      IF v_modulo = 'cre' THEN
         LET v_movimientos = '882,52,732,2,92,112,102,252,1342,1282,912,1042,602,782,72,892,42,482,12,242,542';
      END IF
      LET v_query = "";
      FOREACH 
         SELECT tabla
         INTO   v_tabla
         FROM   cat_tab_movimiento
         LET v_query = TRIM (v_query) || " SELECT COUNT(*) " ||
                                         " FROM   " || v_tabla ||
                                         " WHERE  movimiento in (" || v_movimientos || ") " ||
                                         " AND    id_derechohabiente = " || v_id_derechohabiente ||
                                         " AND    f_liquida <= "  || v_fecha_hasta  ||
                                         " UNION ";
      END FOREACH
      LET v_query = TRIM (v_query) || " SELECT COUNT(*) " ||
                                      " FROM   cta_movimiento " ||
                                      " WHERE  movimiento in (" || v_movimientos || ") " ||
                                      " AND    id_derechohabiente = " || v_id_derechohabiente ||
                                      " AND    f_liquida <= "  || v_fecha_hasta;
   ELSE
      LET v_query = "";      
      FOREACH 
         SELECT tabla
         INTO   v_tabla
         FROM   cat_tab_movimiento
         LET v_query = TRIM (v_query) || " SELECT COUNT(*) " ||
                                         " FROM   " || v_tabla ||
                                         " WHERE  subcuenta in (" || v_subcuenta || ") " ||
                                         " AND    id_derechohabiente = " || v_id_derechohabiente ||
                                         " AND    f_liquida <= "  || v_fecha_hasta  || 
                                         " UNION ";
      END FOREACH
      LET v_query = TRIM (v_query) || " SELECT COUNT(*) " ||
                                      " FROM   cta_movimiento " ||
                                      " WHERE  subcuenta in (" || v_subcuenta || ") " ||
                                      " AND    id_derechohabiente = " || v_id_derechohabiente ||
                                      " AND    f_liquida <= "  || v_fecha_hasta;

   END IF  
   PREPARE stmt_tmp FROM v_query;
   DECLARE cust_cur cursor FOR stmt_tmp;  
   LET v_total_movtos = 0;
   OPEN cust_cur;
      WHILE (1 = 1)
         FETCH cust_cur INTO v_cuenta_movtos;
            IF (SQLCODE != 100) THEN
               LET v_total_movtos = v_total_movtos + v_cuenta_movtos;
            ELSE 
               EXIT WHILE;
            END IF 
      END WHILE
   CLOSE cust_cur;                           
   FREE cust_cur;
   FREE stmt_tmp;

   RETURN v_total_movtos;
END FUNCTION;


