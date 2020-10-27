






CREATE FUNCTION "safreviv".fn_saldo_dia(  p_nss                 CHAR(11),
                               p_id_derechohabiente  DECIMAL(9,0),
                               p_subcuenta           SMALLINT,
                               p_fecha_saldo         DATE

                             )
RETURNING SMALLINT,
          DECIMAL(16,6),
          DECIMAL(16,6);

DEFINE  v_saldo_aivs    DECIMAL(16,6) ;
DEFINE  v_saldo_pesos   DECIMAL(16,6) ;
DEFINE  v_precio_viv    DECIMAL(19,14);
DEFINE  v_precio_fov    DECIMAL(19,14);
DEFINE  v_resultado     SMALLINT;
DEFINE  v_fecha_viv     DATE ;



  -- se asume que la consulta es correcta
  LET v_resultado = 0;


  -- si el id_derechohabiente es nulo, entonces se recibio NSS
  IF ( p_id_derechohabiente IS NULL ) THEN

     -- se obtiene el id_derechohabiente del nss asociado
     SELECT id_derechohabiente
     INTO p_id_derechohabiente
     FROM
        safre_viv:afi_derechohabiente
     WHERE
        nss = p_nss;

   END IF



  -- si se recibe id_derechohabiente
  IF ( p_id_derechohabiente IS NOT NULL ) THEN

     -- si se recibe subcuenta especifica
     IF ( p_subcuenta IS NOT NULL ) THEN
        -- subcuente y fecha especifica
        IF ( p_fecha_saldo IS NOT NULL ) THEN
           SELECT SUM(monto_acciones),
                  SUM(monto_pesos)
           INTO       v_saldo_aivs,
                      v_saldo_pesos
           FROM
              safre_viv:cta_movimiento
           WHERE
                 id_derechohabiente = p_id_derechohabiente
             AND subcuenta          = p_subcuenta
             AND f_liquida         <= p_fecha_saldo;
        ELSE
           --- subcuenta especifica
           SELECT SUM(monto_acciones),
                  SUM(monto_pesos)
           INTO       v_saldo_aivs,
                      v_saldo_pesos
           FROM
              safre_viv:cta_movimiento
           WHERE
                 id_derechohabiente = p_id_derechohabiente
             AND subcuenta          = p_subcuenta;
        END IF
     ELSE
       -- sin subcuenta pero con fecha
       IF ( p_fecha_saldo IS NOT NULL ) THEN
           SELECT SUM(monto_acciones),
                  SUM(monto_pesos)
           INTO       v_saldo_aivs,
                      v_saldo_pesos
           FROM
              safre_viv:cta_movimiento
           WHERE
                 id_derechohabiente = p_id_derechohabiente
             AND f_liquida <= p_fecha_saldo;
       ELSE
          -- suma sin cuenta ni fecha especifica
           SELECT SUM(monto_acciones),
                  SUM(monto_pesos)
           INTO       v_saldo_aivs,
                      v_saldo_pesos
           FROM
              safre_viv:cta_movimiento
           WHERE
                 id_derechohabiente = p_id_derechohabiente;
       END IF
     END IF

  ELSE
     -- no se encontro id_derechohabiente para el NSS dado
     LET v_resultado   = 1;
     LET v_saldo_aivs  = NULL;
     LET v_saldo_pesos = NULL;

  END IF

  -- si no se obtuvieron cifras se regresan en cero
  IF ( v_saldo_aivs is null ) THEN
     LET v_saldo_aivs = 0;
  END IF

  IF ( v_saldo_pesos is null ) THEN
     LET v_saldo_pesos = 0;
  END IF


  -- de devuelve el resultado de la consulta
  RETURN v_resultado, v_saldo_aivs, v_saldo_pesos;
END FUNCTION;


