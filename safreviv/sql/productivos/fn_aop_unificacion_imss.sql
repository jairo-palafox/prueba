






CREATE FUNCTION "safreviv".fn_aop_unificacion_imss()
RETURNING SMALLINT, VARCHAR(100);

  DEFINE v_resultado          SMALLINT;
  DEFINE v_mensaje            VARCHAR(100);

  LET v_resultado = 0;
  LET v_mensaje = "reverso complementario unificacion imss se ejecuto correctamente";

  DROP TABLE IF EXISTS tmp_movimiento_uni_imss CASCADE ;
  CREATE TABLE tmp_movimiento_uni_imss
	(
	  f_liquida 	        DATE,
	  id_derechohabiente 	DECIMAL(9,0),
	  subcuenta 	        SMALLINT,
	  fondo_inversion 	SMALLINT,
	  movimiento 		SMALLINT,
	  folio_liquida 	DECIMAL(9,0),
	  id_referencia 	DECIMAL(9,0),
	  monto_acciones 	DECIMAL(16,6),
	  monto_pesos 		DECIMAL(12,2),
	  f_valor 		DATE,
	  f_registro 		DATE,
	  h_registro 		DATETIME HOUR TO SECOND,
	  origen 		CHAR(20)
	) in uni_dbs  
	 extent size 16 next size 16 lock mode row;

  DROP TABLE IF EXISTS tmp_mov_uni_marca_imss CASCADE ;

  CREATE TABLE tmp_mov_uni_marca_imss
	(
	  f_liquida 	        DATE,
	  id_derechohabiente 	DECIMAL(9,0),
	  subcuenta 	        SMALLINT,
	  fondo_inversion 	SMALLINT,
	  movimiento 		SMALLINT,
	  folio_liquida 	DECIMAL(9,0),
	  id_referencia 	DECIMAL(9,0),
	  monto_acciones 	DECIMAL(16,6),
	  monto_pesos 		DECIMAL(12,2),
	  f_valor 		DATE,
	  f_registro 		DATE,
	  h_registro 		DATETIME HOUR TO SECOND,
	  origen 		CHAR(20)
	) in uni_dbs  
	 extent size 16 next size 16 lock mode row;

      INSERT INTO tmp_movimiento_uni_imss
      SELECT *
        FROM cta_movimiento
       WHERE folio_liquida = 21285;

      INSERT INTO tmp_mov_uni_marca_imss
       SELECT t_mov.*
        FROM safre_tmp:tmp_movimiento_uni t_mov;

       DELETE 
         FROM cta_movimiento
        WHERE folio_liquida = 21285
          AND id_derechohabiente IN
                                   ( SELECT id_derechohabiente
                                       FROM safre_tmp:tmp_id_do
                                   )
          AND id_derechohabiente NOT IN (30173792,6339634) ;

       DELETE 
         FROM cta_movimiento
        WHERE folio_liquida = 21285
          AND id_Derechohabiente IN (
                                     SELECT id_derechohabiente
                                       FROM safre_tmp:tmp_id_dor)
          AND origen[1,11] IN (
                               SELECT a.nss
                                 FROM afi_derechohabiente a,
                                      safre_tmp:tmp_id_do u
                                WHERE a.id_derechohabiente = u.id_derechohabiente)
          AND id_derechohabiente NOT IN (30173792,6339634);

   RETURN v_resultado, v_mensaje;
END FUNCTION;


