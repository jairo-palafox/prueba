DROP FUNCTION IF EXISTS fn_consulta_convivencia;
CREATE FUNCTION fn_consulta_convivencia(p_nss                 CHAR(11),
                                        p_id_derechohabiente  DECIMAL(9,0),
                                        p_marca               SMALLINT
                             )
RETURNING SMALLINT,
          CHAR(40);

DEFINE v_rch_cod  SMALLINT;
DEFINE v_rch_desc CHAR(40);



    LET v_rch_cod  = 0;
    LET v_rch_desc = "";

    -- si el id_derechohabiente es nulo, entonces se recibio NSS
    IF ( p_id_derechohabiente IS NULL OR p_id_derechohabiente = 0) THEN

        -- se obtiene el id_derechohabiente del nss asociado
        SELECT id_derechohabiente
        INTO   p_id_derechohabiente
        FROM   afi_derechohabiente
        WHERE  nss = p_nss;
    END IF

    -- si se recibe id_derechohabiente
    IF ( p_id_derechohabiente IS NOT NULL ) THEN

        -- se busca la marca
        IF ( p_marca IS NOT NULL ) THEN
	        SELECT FIRST 1 c.rch_cod, NVL(d.rch_desc,'MARCA NO CONVIVE')
 	        INTO   v_rch_cod, v_rch_desc
	        FROM   sfr_convivencia AS c 
	        INNER JOIN sfr_marca_activa AS a ON a.marca = c.marca_activa
	        LEFT JOIN cat_rch_marca AS d ON c.rch_cod = d.rch_cod
	        WHERE  a.id_derechohabiente = p_id_derechohabiente
	        AND    a.marca              = c.marca_activa
	        AND    c.marca_entra        = p_marca
	        AND    c.rch_cod            > 0;
        END IF 
    END IF 
    IF v_rch_cod IS NULL THEN 
        LET v_rch_cod = 0;
    END IF 
    
RETURN v_rch_cod, v_rch_desc;
END FUNCTION;                                                                                                                                                                                                                                                