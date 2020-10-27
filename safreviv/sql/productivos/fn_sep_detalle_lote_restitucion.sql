






CREATE FUNCTION "safreviv".fn_sep_detalle_lote_restitucion(p_folio DECIMAL(9,0))
RETURNING  INTEGER,
           INTEGER,
           CHAR(254),
           DECIMAL(9,0),
           CHAR(11),
           DATE,
           DECIMAL(16,6),
		   DECIMAL(12,2),
           DECIMAL(16,6),
		   DECIMAL(12,2),
           DATE,
           DECIMAL(16,6),
		   DECIMAL(12,2),
           DECIMAL(16,6),
		   DECIMAL(12,2),
           --DATE,
           DECIMAL(16,6),
		   DECIMAL(12,2),
           DECIMAL(16,6),
		   DECIMAL(12,2),
		   DECIMAL(12,2),
		   DECIMAL(12,2);
                                         
DEFINE v_id_expediente      	DECIMAL(9,0);
DEFINE v_nss                	CHAR(11);
DEFINE v_id_derechohabiente 	DECIMAL(9,0);
DEFINE v_f_val_deudor       	DATE;
DEFINE v_mov92_deudor       	DECIMAL(16,6);
DEFINE v_mov92_deudor_pesos 	DECIMAL(12,2);
DEFINE v_mov97_deudor       	DECIMAL(16,6);
DEFINE v_mov97_deudor_pesos 	DECIMAL(12,2);
DEFINE v_f_val_liquidado    	DATE;
DEFINE v_mov92_liquidado    	DECIMAL(16,6);
DEFINE v_mov92_liquidado_pesos	DECIMAL(12,2);
DEFINE v_mov97_liquidado    	DECIMAL(16,6);
DEFINE v_mov97_liquidado_pesos  DECIMAL(12,2);
DEFINE v_f_val_procesar     DATE;
DEFINE v_mov92_procesar     	DECIMAL(16,6);
DEFINE v_mov92_procesar_pesos   DECIMAL(12,2);
DEFINE v_mov97_procesar     	DECIMAL(16,6);
DEFINE v_mov97_procesar_pesos   DECIMAL(12,2);
DEFINE v_int_92                 DECIMAL(12,2);
DEFINE v_int_97                 DECIMAL(12,2);
--Temporales
DEFINE tab_name VARCHAR(20);
DEFINE v_mov92_liquidado_temp   	DECIMAL(16,6);
DEFINE v_mov92_liquidado_pesos_temp DECIMAL(12,2);
DEFINE v_mov97_liquidado_temp 		DECIMAL(16,6);
DEFINE v_mov97_liquidado_pesos_temp DECIMAL(12,2);

DEFINE v_error_sql  INTEGER;
DEFINE v_isam_error INTEGER;
DEFINE v_msg_error  CHAR(254);

   ON EXCEPTION SET v_error_sql,
                    v_isam_error,
                    v_msg_error
             
      LET v_f_val_deudor    = NULL;
      LET v_mov92_deudor    = 0;
	  LET v_mov92_deudor_pesos = 0;
      LET v_mov97_deudor    = 0;
	  LET v_mov97_deudor_pesos = 0;
      LET v_f_val_liquidado = NULL;
      LET v_mov92_liquidado = 0;
	  LET v_mov92_liquidado_pesos = 0;
      LET v_mov97_liquidado = 0;
	  LET v_mov97_liquidado_pesos = 0;
      --LET v_f_val_procesar  = NULL;
      LET v_mov92_procesar  = 0;
	  LET v_mov92_procesar_pesos  = 0;
      LET v_mov97_procesar  = 0;
	  LET v_mov97_procesar_pesos  = 0;
          LET v_int_92 = 0;
          LET v_int_97 = 0;
                    
      RETURN v_error_sql,
             v_isam_error,
             v_msg_error,
             v_id_expediente,
             v_nss,
             v_f_val_deudor,
             v_mov92_deudor,
			 v_mov92_deudor_pesos,
             v_mov97_deudor,
			 v_mov97_deudor_pesos,
             v_f_val_liquidado,
             v_mov92_liquidado,
			 v_mov92_liquidado_pesos,
             v_mov97_liquidado,
			 v_mov97_liquidado_pesos,
             --v_f_val_procesar,
             v_mov92_procesar,
			 v_mov92_procesar_pesos,
             v_mov97_procesar,
			 v_mov97_procesar_pesos ,
                         v_int_92,
                         v_int_92;
                    
   END EXCEPTION
   
   LET v_error_sql  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = "Consulta realizada correctamente";
      
   LET v_id_expediente      = 0;
   LET v_id_derechohabiente = 0;
   LET v_nss                = 0;
   LET v_f_val_deudor       = NULL;
   LET v_mov92_deudor       = 0;
   LET v_mov92_deudor_pesos = 0;
   LET v_mov97_deudor       = 0;
   LET v_mov97_deudor_pesos = 0;
   LET v_f_val_liquidado    = NULL;
   LET v_mov92_liquidado    = 0;
   LET v_mov92_liquidado_pesos = 0;
   LET v_mov97_liquidado    = 0;
   LET v_mov97_liquidado_pesos = 0;
   --LET v_f_val_procesar     = NULL;
   LET v_mov92_procesar     = 0;
   LET v_mov92_procesar_pesos = 0;
   LET v_mov97_procesar     = 0;
   LET v_mov97_procesar_pesos = 0;
   LET v_int_92 = 0;
   LET v_int_97 = 0;
   
   FOREACH SELECT UNIQUE exp.id_expediente,
                  exp.id_derechohabiente,
                  exp.nss
             INTO v_id_expediente,
                  v_id_derechohabiente,
                  v_nss
             FROM sep_mov_deudor deu JOIN sep_nss_expediente exp
               ON exp.id_derechohabiente = deu.id_derechohabiente
              AND exp.id_expediente = deu.id_referencia
            WHERE deu.folio_liquida = p_folio
              AND movimiento = 381 -- Sólo los abonos por restitución, no se debe considerar los cargos por restitución (382) que probienen de la compensación del deudor
            ORDER BY 1
      
      LET v_mov92_deudor = NULL;
      LET v_mov97_deudor = NULL;
      LET v_f_val_deudor = NULL;
      -- Recupera monto total para el derechohabiente en restitución
      SELECT NVL(SUM(CASE subcuenta WHEN 8 THEN monto_aivs END),0), -- sólo subcuenta de viv 92
             NVL(SUM(CASE subcuenta WHEN 8 THEN monto_pesos END),0),
			 NVL(SUM(CASE subcuenta WHEN 4 THEN monto_aivs END),0), -- sólo subcuenta de viv 92
             NVL(SUM(CASE subcuenta WHEN 4 THEN monto_pesos END),0),
			 NVL(f_movimiento,NULL) -- fecha del valor de cotización
        INTO v_mov92_deudor,
             v_mov92_deudor_pesos,
			 v_mov97_deudor,
			 v_mov97_deudor_pesos,
             v_f_val_deudor
        FROM dse_devolucion
       WHERE folio_referencia = p_folio
         AND id_derechohabiente = v_id_derechohabiente
       GROUP BY 5;
      IF(v_mov92_deudor IS NULL)THEN
         LET v_mov92_deudor = 0;
		 LET v_mov92_deudor_pesos = 0;
      END IF
      IF(v_mov97_deudor IS NULL)THEN
         LET v_mov97_deudor = 0;
		 LET v_mov97_deudor_pesos = 0;
      END IF

   	LET v_mov92_liquidado       = 0;
	LET v_mov92_liquidado_pesos	= 0 ;
    LET v_mov97_liquidado 		= 0;
	LET v_mov97_liquidado_pesos = 0 ;
	LET v_f_val_liquidado       = 0;
	LET v_mov92_liquidado_temp       = 0 ;
	LET v_mov92_liquidado_pesos_temp = 0 ;
    LET v_mov97_liquidado_temp 		 = 0 ;
	LET v_mov97_liquidado_pesos_temp = 0 ;

      -- Recupera monto total para el derechohabiente en liquidación devolución
   -- Recupera monto total para el lote en liquidación devolución
   FOREACH SELECT tabname INTO tab_name FROM systables
   WHERE tabname matches "cta_movimiento*"
   AND  tabname not in ("cta_movimiento_total")
      ORDER BY created ASC
	  
      PREPARE prp_detalle FROM "SELECT NVL(SUM(CASE mov.subcuenta WHEN 8 THEN mov.monto_acciones END),0), NVL(SUM(CASE mov.subcuenta WHEN 8 THEN mov.monto_pesos END),0), NVL(SUM(CASE mov.subcuenta WHEN 4 THEN mov.monto_acciones END),0), NVL(SUM(CASE mov.subcuenta WHEN 4 THEN mov.monto_pesos END),0), NVL(mov.f_valor,'') FROM TABLE(MULTISET(SELECT id_derechohabiente,folio  FROM dse_devolucion WHERE folio_referencia = ? AND id_derechohabiente = ? GROUP BY 1,2)) dev JOIN dse_agrupa_devolucion gpo ON gpo.id_derechohabiente = dev.id_derechohabiente AND gpo.nss_separacion <> ''  JOIN dse_his_devolucion his ON his.id_dse_grp_devolucion = gpo.id_dse_grp_devolucion AND his.folio = dev.folio JOIN " || tab_name || " mov ON mov.id_derechohabiente = gpo.id_derechohabiente AND mov.folio_liquida = gpo.folio_liquida AND mov.movimiento <> 1099 GROUP BY 5;";
      DECLARE cur CURSOR FOR prp_detalle;
      OPEN cur USING p_folio,v_id_derechohabiente;
      FETCH cur INTO v_mov92_liquidado_temp,
				   v_mov92_liquidado_pesos_temp,
                   v_mov97_liquidado_temp,
				   v_mov97_liquidado_pesos_temp,
				   v_f_val_liquidado;
          IF SQLCODE == 0 THEN
             LET v_mov92_liquidado       = v_mov92_liquidado       + v_mov92_liquidado_temp;
             LET v_mov92_liquidado_pesos = v_mov92_liquidado_pesos + v_mov92_liquidado_pesos_temp;
             LET v_mov97_liquidado       = v_mov97_liquidado       + v_mov97_liquidado_temp;
             LET v_mov97_liquidado_pesos = v_mov97_liquidado_pesos + v_mov97_liquidado_pesos_temp;
          END IF;
      CLOSE cur;
      FREE cur;
      FREE prp_detalle;
	  
	END FOREACH;
      
      IF(v_mov92_liquidado IS NULL)THEN
         LET v_mov92_liquidado = 0;
		 LET v_mov92_liquidado_pesos = 0;
      END IF
      IF(v_mov97_liquidado IS NULL)THEN
         LET v_mov97_liquidado = 0;
		 LET v_mov97_liquidado_pesos = 0;
      END IF
	  
      LET v_mov92_procesar = NULL;
      LET v_mov97_procesar = NULL;
      --LET v_f_val_procesar = NULL;
      -- Recupera monto total para el derechohabiente en devolución procesar
      -- 1.-Hay que agrupar dse_devolucion para obtener id_derechohabientes únicos y evitar valores errónes
      
      -- 2.-Primero encuentra el registro enviado en dse_his_devolucion y luego lo relaciona con la misma tabla para encontrar
      -- el devuelto por pocesar
      SELECT NVL(SUM(hisdo.aivs92),0),
			 NVL(SUM(hisdo.pesos92),0),
             NVL(SUM(hisdo.aivs97),0),
			 NVL(SUM(hisdo.pesos97),0)
             --NVL(his.f_proceso,NULL)
        INTO v_mov92_procesar,
             v_mov92_procesar_pesos,
			 v_mov97_procesar,
			 v_mov97_procesar_pesos
             --v_f_val_procesar
        FROM TABLE(MULTISET(SELECT id_derechohabiente,folio 
             FROM dse_devolucion WHERE folio_referencia = p_folio AND id_derechohabiente = v_id_derechohabiente GROUP BY 1,2)) dev --1.-
             JOIN dse_agrupa_devolucion gpo
          ON gpo.id_derechohabiente = dev.id_derechohabiente
         AND gpo.nss_separacion <> ''  -- Filtro para recuperar sólo movimientos relacionados a separación y para el folio indicado (de separación)
             JOIN dse_his_devolucion his
          ON his.id_dse_grp_devolucion = gpo.id_dse_grp_devolucion
         AND his.folio = dev.folio
             JOIN dse_his_devolucion hisdo -- 2.-
          ON hisdo.id_dse_grp_devolucion = his.id_dse_grp_devolucion
         AND hisdo.folio > dev.folio
         AND hisdo.edo_procesar = 120; -- Confirmados por procesar
       --GROUP BY 3;
       
      IF(v_mov92_procesar IS NULL)THEN
         LET v_mov92_procesar = 0;
		 LET v_mov92_procesar_pesos = 0;
      END IF
      IF(v_mov97_procesar IS NULL)THEN
         LET v_mov97_procesar = 0;
		 LET v_mov97_procesar_pesos = 0;
      END IF
     

-- se calcula el rendimiento del 1ero de mes al 12 hábil del mes


     LET v_f_val_procesar = MDY(MONTH(v_f_val_liquidado),"01",YEAR(v_f_val_liquidado)); 

     SELECT (v_mov92_liquidado_pesos - (v_mov92_liquidado * a.precio_fondo)) ,
            (v_mov97_liquidado_pesos - (v_mov97_liquidado * a.precio_fondo)) 
     INTO   v_int_92  ,
            v_int_97
     FROM   glo_valor_fondo a 
     WHERE  a.f_valuacion = v_f_val_procesar
     AND    a.fondo = 11;

     IF v_int_92 IS NULL THEN LET v_int_92 = 0; END IF
     IF v_int_97 IS NULL THEN LET v_int_97 = 0; END IF


      RETURN v_error_sql,
             v_isam_error,
             v_msg_error,
             v_id_expediente,
             v_nss,
             v_f_val_deudor,
             v_mov92_deudor,
			 v_mov92_deudor_pesos,
             v_mov97_deudor,
			 v_mov97_deudor_pesos,
             v_f_val_liquidado,
             v_mov92_liquidado,
			 v_mov92_liquidado_pesos,
             v_mov97_liquidado,
			 v_mov97_liquidado_pesos,
             --v_f_val_procesar,
             v_mov92_procesar,
			 v_mov92_procesar_pesos,
             v_mov97_procesar,
			 v_mov97_procesar_pesos ,
                         v_int_92 , 
                         v_int_97 WITH RESUME;
          
   END FOREACH
   
END FUNCTION;


