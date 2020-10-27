






CREATE FUNCTION "safreviv".fn_valida_instruccion_mdt_can(p_id_origen  			   	    LIKE mdt_solicitud_mandato.id_origen,
		                                          p_nss                 	  LIKE mdt_solicitud_mandato.nss,
		                                          p_id_credito              LIKE mdt_solicitud_mandato.id_credito,
		                                          --p_id_mandato              LIKE mdt_solicitud_mandato.id_mandato,
		                                          p_cve_mandato             LIKE mdt_solicitud_mandato.cve_mandato,
		                                          p_tpo_descuento_mandato   LIKE mdt_solicitud_mandato.tpo_descuento_mandato,
		                                          p_valor_descuento_mandato LIKE mdt_solicitud_mandato.valor_descuento_mandato,
		                                          p_f_canales               LIKE mdt_solicitud_mandato.f_canales,
		                                          p_f_inicio_mandato        LIKE mdt_solicitud_mandato.f_inicio_mandato,
		                                          p_f_culmina_mandato       LIKE mdt_solicitud_mandato.f_culmina_mandato,
		                                          p_referencia              LIKE mdt_solicitud_mandato.referencia,
		                                          p_id_canales              LIKE mdt_solicitud_mandato.id_canales,
		                                          p_tipo_operacion          LIKE mdt_solicitud_mandato.tipo_operacion)
RETURNING CHAR(3);

DEFINE v_id_derechohabiente	LIKE afi_derechohabiente.id_derechohabiente;
DEFINE v_num_credito			  LIKE cta_credito.num_credito;
	
	--Se valida el campo de id_origen
	IF p_id_origen <> 2 THEN
		RETURN "010";
	END IF
	
	--Se valida que el nss se encuentre en la tabla de afi_derechohabiente
	SELECT id_derechohabiente
	INTO   v_id_derechohabiente
	FROM afi_derechohabiente
	WHERE nss = p_nss;

	IF v_id_derechohabiente IS NULL THEN
		RETURN "020";
	END IF

	--Se valida que exista el credito en la tabla cta_credito
	SELECT num_credito
	INTO v_num_credito
	FROM cta_credito
	WHERE id_derechohabiente = v_id_derechohabiente
	AND tpo_credito = 1;		--Credito tradicional

	IF v_num_credito IS NULL  OR v_num_credito <> p_id_credito THEN
		RETURN "030";
	END IF

	--Se valida que exista el id_mandato en la tabla mdt_cat_mandato
	IF NOT EXISTS(
				SELECT cat.cve_mandato 
          FROM mdt_cat_mandato cat JOIN mdt_cat_mandato_paquete paq
            ON cat.id_cat_mandato = paq.id_cat_mandato
         WHERE paq.cve_mandato = p_cve_mandato
			   ) THEN 
		RETURN "040";
	END IF

	--Validacion del campo tpo_descuento
	IF p_tpo_descuento_mandato <> 3 THEN
		RETURN "050";
	END IF

	--Se valida que para el tipo_operacion = A o M el valor_descuento sea mayor a cero
	IF (p_tipo_operacion = 'A' OR p_tipo_operacion = 'M') AND p_valor_descuento_mandato <= 0 THEN
		RETURN "051";
	END IF

	--Valida que la f_inicio_mandato sea una fecha valida verificando que el campo sea distinto a nulo
	IF p_f_inicio_mandato IS NULL THEN
		RETURN "060";
	END IF

	--Valida que la f_culmina_mandato sea una fecha valida verificando que el campo sea distinto a nulo
	IF p_f_culmina_mandato IS NULL THEN
		RETURN "060";
	END IF

	--Valida que la referencia no sea nula
	IF p_referencia IS NULL THEN
		RETURN "071";
	END IF

	--Valida que id_canales no sea nulo
	IF p_id_canales IS NULL THEN
		RETURN "080";
	END IF

	--Valida que el tipo_operacion contenga valores validos
	IF p_tipo_operacion <> 'A' AND p_tipo_operacion <> 'B' AND p_tipo_operacion <> 'M' AND p_tipo_operacion <> 'R' THEN
		RETURN "090";
	END IF

	--En este punto ya paso todas las validaciones de negocio por lo que se inserta el registro en la tabla mdt_solicitud_mandato
	INSERT INTO mdt_solicitud_mandato VALUES (seq_mdt_solicitud_mandato.NEXTVAL, --id_solicitud_mandato
                                				    v_id_derechohabiente,						   --id_derechohabiente
                                				    p_id_origen,								       --id_origen
                                				    NULL,									             --f_lote
                                				    --NULL,									             --lote
                                				    --NULL,									             --id_lote
                                				    p_nss,									           --nss
                                				    p_id_credito,								       --id_credito
                                				    --p_id_mandato,								       --id_mandato
                                				    p_tpo_descuento_mandato,					 --id_mandato
                                				    p_valor_descuento_mandato,				 --tpo_descuento
                                				    p_f_inicio_mandato,							   --f_inicio_mandato
                                				    p_f_culmina_mandato,						   --f_culmina_mandato
                                				    p_referencia,								       --referencia
                                				    p_cve_mandato,
                                				    41,										             --scta_origen (amortización)
                                				    1,										             --modalidad_aplicacion (bimestral)
                                				    'servicios',								       --usuario
                                				    101,										           --estado
                                				    p_f_canales,								       --f_canales
                                				    p_id_canales,								       --id_canales
                                				    p_tipo_operacion,							     --tipo_operacion
                                				    NULL										           --diagnostico
                                				    );
	RETURN "000";
	
END FUNCTION;


