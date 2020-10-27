






CREATE FUNCTION "safreviv".fn_sep_cifras_totales_lote_restitucion(p_folio DECIMAL(9,0))
--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 3 de junio de 2015
-- Recupera las cifras globales de restitución deudor para un  
-- folio especifico.
--===============================================================
RETURNING INTEGER,
          INTEGER,
          CHAR(254),
          DATE,
          DATE,
          DECIMAL(16,6),
		  DECIMAL(12,2), --NUEVO, MONTO EN PESOS
          DECIMAL(16,6),
		  DECIMAL(12,2), --NUEVO, MONTO EN PESOS
          DATE,
          DECIMAL(16,6),
          DECIMAL(12,2), --NUEVO
		  DECIMAL(16,6),
		  DECIMAL(12,2), --NUEVO
          --DATE,
          DECIMAL(16,6),
		  DECIMAL(12,2), --NUEVO
          DECIMAL(16,6),
		  DECIMAL(12,2),  --NUEVO
		  DECIMAL(12,2),  --NUEVO
		  DECIMAL(12,2);  --NUEVO
           
DEFINE v_fecha_registro  		DATE;
DEFINE v_f_val_deudor    		DATE;
DEFINE v_mov92_deudor          	DECIMAL(16,6);
DEFINE v_mov92_deudor_pesos    	DECIMAL(12,2); --NUEVO, MONTO EN PESOS
DEFINE v_mov97_deudor    		DECIMAL(16,6);
DEFINE v_mov97_deudor_pesos    	DECIMAL(12,2); --NUEVO, MONTO EN PESOS
DEFINE v_f_val_liquidado 		DATE;
DEFINE v_mov92_liquidado 		DECIMAL(16,6);
DEFINE v_mov92_liquidado_pesos 	DECIMAL(12,2); --Nuevo
DEFINE v_mov97_liquidado 		DECIMAL(16,6);
DEFINE v_mov97_liquidado_pesos 	DECIMAL(12,2); --Nuevo
DEFINE v_f_val_procesar  DATE;
DEFINE v_mov92_procesar  		DECIMAL(16,6);
DEFINE v_mov92_procesar_pesos  	DECIMAL(12,2); --Nuevo
DEFINE v_mov97_procesar  		DECIMAL(16,6);
DEFINE v_mov97_procesar_pesos  	DECIMAL(12,2); --Nuevo
--Temporales
DEFINE tab_name VARCHAR(20);
DEFINE v_mov92_liquidado_temp   	DECIMAL(16,6);
DEFINE v_mov92_liquidado_pesos_temp DECIMAL(12,2);
DEFINE v_mov97_liquidado_temp 		DECIMAL(16,6);
DEFINE v_mov97_liquidado_pesos_temp DECIMAL(12,2);
DEFINE v_int_92                     DECIMAL(12,2);
DEFINE v_int_97                     DECIMAL(12,2);

DEFINE v_error_sql  INTEGER;
DEFINE v_isam_error INTEGER;
DEFINE v_msg_error  CHAR(254);

   ON EXCEPTION SET v_error_sql,
                    v_isam_error,
                    v_msg_error
             
      LET v_fecha_registro  = NULL;
      LET v_f_val_deudor    = NULL;
      LET v_mov92_deudor    = 0;
	  LET v_mov92_deudor_pesos = 0; --nuevo
      LET v_mov97_deudor    = 0;
	  LET v_mov97_deudor_pesos = 0; --nuevo
      LET v_f_val_liquidado = NULL;
      LET v_mov92_liquidado = 0;
	  LET v_mov92_liquidado_pesos = 0; --nuevo
      LET v_mov97_liquidado = 0;
	  LET v_mov97_liquidado_pesos = 0; --nuevo
      --LET v_f_val_procesar  = NULL;
      LET v_mov92_procesar  = 0;
	  LET v_mov92_procesar_pesos  = 0; --nuevo
      LET v_mov97_procesar  = 0;
	  LET v_mov97_procesar_pesos  = 0; --nuevo
          LET v_int_92 = 0;
          LET v_int_97 = 0;
                    
      RETURN v_error_sql,
             v_isam_error,
             v_msg_error,
             v_fecha_registro,
             v_f_val_deudor,
             v_mov92_deudor,
			 v_mov92_deudor_pesos, --nuevo
             v_mov97_deudor,
             v_mov97_deudor_pesos, --nuevo
			 v_f_val_liquidado,
             v_mov92_liquidado,
			 v_mov92_liquidado_pesos,  --nuevo
             v_mov97_liquidado,
			 v_mov97_liquidado_pesos,  --nuevo
             --v_f_val_procesar,
             v_mov92_procesar,
			 v_mov92_procesar_pesos, --nuevo
             v_mov97_procesar,
			 v_mov97_procesar_pesos,
                         v_int_92,
                         v_int_97; --nuevo
                    
   END EXCEPTION
   
   LET v_error_sql  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = "Consulta realizada correctamente";
      
   LET v_fecha_registro  = NULL;
   LET v_f_val_deudor    = NULL;
   LET v_mov92_deudor    = 0;
   LET v_mov92_deudor_pesos = 0; --nuevo
   LET v_mov97_deudor    = 0;
   LET v_mov97_deudor_pesos = 0; --nuevo
   LET v_f_val_liquidado = NULL;
   LET v_mov92_liquidado = 0;
   LET v_mov92_liquidado_pesos = 0; --N
   LET v_mov97_liquidado = 0;
   LET v_mov97_liquidado_pesos = 0; --N
   --LET v_f_val_procesar  = NULL;
   LET v_mov92_procesar  = 0;
   LET v_mov92_procesar_pesos  = 0; --N
   LET v_mov97_procesar  = 0;
   LET v_mov97_procesar_pesos  = 0; --N
          LET v_int_92 = 0;
          LET v_int_97 = 0;
   
   SELECT DISTINCT f_registro
     INTO v_fecha_registro
     FROM sep_mov_deudor
    WHERE folio_liquida = p_folio
      AND movimiento = 381; -- Sólo los abonos por restitución, no se debe considerar los cargos por restitución (382) que probienen de la compensación del deudor

   -- Recupera monto total para el lote en restitución
   SELECT NVL(SUM(CASE subcuenta WHEN 8 THEN round(monto_aivs,2) END),0), -- sólo subcuenta de viv 92
          NVL(SUM(CASE subcuenta WHEN 8 THEN round(monto_pesos,2) END),0), -- Su monto en pesos
		  NVL(SUM(CASE subcuenta WHEN 4 THEN round(monto_aivs,2) END),0), -- sólo subcuenta de viv 97
		  NVL(SUM(CASE subcuenta WHEN 4 THEN round(monto_pesos,2) END),0), -- su monto en pesos
          NVL(f_movimiento,'') -- fecha del valor de cotización
     INTO v_mov92_deudor,
		  v_mov92_deudor_pesos,
          v_mov97_deudor,
          v_mov97_deudor_pesos,
		  v_f_val_deudor
     FROM dse_devolucion
    WHERE folio_referencia = p_folio
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
   -- Recupera monto total para el lote en liquidación devolución
   FOREACH SELECT tabname INTO tab_name FROM systables
    WHERE tabname matches "cta_movimiento*"
     AND tabname not in ("cta_movimiento_total")
      ORDER BY created ASC
	  
      PREPARE prp_cifras FROM "SELECT NVL(SUM(CASE mov.subcuenta WHEN 8 THEN round(mov.monto_acciones,2) END),0), NVL(SUM(CASE mov.subcuenta WHEN 8 THEN round(mov.monto_pesos,2) END),0), NVL(SUM(CASE mov.subcuenta WHEN 4 THEN round(mov.monto_acciones,2) END),0), NVL(SUM(CASE mov.subcuenta WHEN 4 THEN round(mov.monto_pesos,2) END),0), NVL(mov.f_valor,'') FROM TABLE(MULTISET(SELECT id_derechohabiente,folio FROM dse_devolucion WHERE folio_referencia = ? GROUP BY 1,2)) dev JOIN dse_agrupa_devolucion gpo ON gpo.id_derechohabiente = dev.id_derechohabiente AND gpo.nss_separacion <> '' JOIN dse_his_devolucion his ON his.id_dse_grp_devolucion = gpo.id_dse_grp_devolucion AND his.folio = dev.folio JOIN " || tab_name || " mov ON mov.id_derechohabiente = gpo.id_derechohabiente AND mov.folio_liquida = gpo.folio_liquida AND mov.movimiento <> 1099 GROUP BY 5;";
      DECLARE cur CURSOR FOR prp_cifras;
      OPEN cur USING p_folio;
      FETCH cur INTO v_mov92_liquidado_temp,
				   v_mov92_liquidado_pesos_temp,
                   v_mov97_liquidado_temp,
				   v_mov97_liquidado_pesos_temp,
				   v_f_val_liquidado;
	  
	  IF SQLCODE == 0 THEN  
	    LET v_mov92_liquidado       	= v_mov92_liquidado 	  + v_mov92_liquidado_temp;
	    LET v_mov92_liquidado_pesos	    = v_mov92_liquidado_pesos + v_mov92_liquidado_pesos_temp;
        LET v_mov97_liquidado 			= v_mov97_liquidado 	  + v_mov97_liquidado_temp;
	    LET v_mov97_liquidado_pesos 	= v_mov97_liquidado_pesos + v_mov97_liquidado_pesos_temp;
	  END IF;
	  
	  CLOSE cur;
      FREE cur;
      FREE prp_cifras;
		  
	END FOREACH;
	
   IF(v_mov92_liquidado IS NULL)THEN
      LET v_mov92_liquidado = 0;
	  LET v_mov92_liquidado_pesos = 0; 
   END IF
   IF(v_mov97_liquidado IS NULL)THEN
      LET v_mov97_liquidado = 0;
      LET v_mov97_liquidado_pesos = 0;	  
   END IF
    
	
   LET v_mov92_procesar = 0;
   LET v_mov97_procesar = 0;
   -- Recupera monto total para el lote en devolución procesar
   -- 1.- Hay que agrupar dse_devolucion para obtener id_derechohabientes únicos y evitar valores errones
   
   -- 2.- Primero encuentra el registro enviado en dse_his_devolucion y luego lo relaciona con la misma tabla para encontrar
   -- el devuelto por pocesar
   SELECT NVL(SUM(round(hisdo.aivs92,2)),0),
          NVL(SUM(round(hisdo.pesos92,2)),0),
		  NVL(SUM(round(hisdo.aivs97,2)),0),
		  NVL(SUM(round(hisdo.pesos97,2)),0)
          --NVL(his.f_proceso,'')
     INTO v_mov92_procesar,
		  v_mov92_procesar_pesos,
          v_mov97_procesar,
		  v_mov97_procesar_pesos
          --v_f_val_procesar
     FROM TABLE(MULTISET(SELECT id_derechohabiente,folio 
          FROM dse_devolucion WHERE folio_referencia = p_folio GROUP BY 1,2)) dev -- 1.-
          JOIN dse_agrupa_devolucion gpo
       ON gpo.id_derechohabiente = dev.id_derechohabiente
      AND gpo.nss_separacion <> ''
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
   
  LET v_f_val_procesar = MDY(MONTH(v_f_val_liquidado),"01",YEAR(v_f_val_liquidado));

  SELECT v_mov92_liquidado_pesos - (v_mov92_liquidado * a.precio_fondo) ,
         v_mov97_liquidado_pesos - (v_mov97_liquidado * a.precio_fondo)
  INTO   v_int_92, 
         v_int_97 
  FROM   glo_valor_fondo a  
  WHERE  fondo = 11 
  AND    f_valuacion = v_f_val_procesar;

  IF v_int_92 IS NULL THEN LET v_int_92 = 0; END IF
  IF v_int_97 IS NULL THEN LET v_int_97 = 0; END IF

   RETURN v_error_sql,
          v_isam_error,
          v_msg_error,
          v_fecha_registro,
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
		  v_mov97_procesar_pesos,
                  v_int_92 ,
                  v_int_97;
          
END FUNCTION;


