






CREATE FUNCTION "safreviv".fn_ret_datamart_notificacion_historico(p_folio_notificacion DECIMAL(9,0))

	RETURNING INTEGER, INTEGER, VARCHAR(250)

    -- Variables de control
    DEFINE v_num_notificaciones  INTEGER;
    -- Numero de notificaciones que se guardaron con estado 4
    -- y ahora convive la marca
    DEFINE v_sec_pension         SMALLINT;
    DEFINE v_notificacion        INTEGER;
    DEFINE v_notificacion_tmp    INTEGER;
    DEFINE v_id_datamart_cont    DECIMAL(9,0);

    -- Variables a insertar en ret_notificacion
    DEFINE v_id_datamart         DECIMAL(9,0);
    DEFINE v_nss                 CHAR(11);
    DEFINE v_id_derechohabiente  DECIMAL(9,0);
    DEFINE v_ind_envio           SMALLINT;
    
    -- Control de Excepciones
	DEFINE r_sql_err                 INTEGER;
	DEFINE r_isam_err                INTEGER;
	DEFINE r_err_txt                 VARCHAR(250);
	
	-- Variables para saldo
    DEFINE v_resultado            SMALLINT;
    DEFINE v_saldo_aivs           DECIMAL(16,6);
    DEFINE v_saldo_pesos          DECIMAL(16,6);
    DEFINE v_suma_vivienda_92_97   DECIMAL(16,6);
    
    -- Variables de control para marcas
    DEFINE v_rch_cod             SMALLINT;
    DEFINE v_rch_desc            CHAR(40);
    
    -- se configura el retorno de los valores
   	ON EXCEPTION SET r_sql_err, r_isam_err, r_err_txt 
		LET v_num_notificaciones = r_sql_err;

		RETURN v_num_notificaciones, r_isam_err, r_err_txt;
	END EXCEPTION

	-- Se incializan las variables
    LET v_num_notificaciones = 0;
    LET r_sql_err            = 0;
    LET r_isam_err           = 0;
    LET r_err_txt            = "";

    DROP TABLE IF EXISTS tmp_ret_notificacion;
    
    CREATE TABLE tmp_ret_notificacion(                  
                        id_datamart         DECIMAL(9,0),
                        folio_notificacion  DECIMAL(9,0),
                        nss                 CHAR(11)    ,
                        id_derechohabiente  DECIMAL(9,0),
                        ind_envio           SMALLINT    ,
                        f_envio             DATE        ,
                        folio_datamart      DECIMAL(9,0));
    
    FOREACH SELECT DISTINCT rd.nss,
                   ad.id_derechohabiente,
                   MAX(rd.sec_pension)
    	      INTO v_nss,
       		       v_id_derechohabiente,
       		       v_sec_pension
			FROM   ret_datamart rd
       			   JOIN afi_derechohabiente ad
                        ON rd.nss = ad.nss
			WHERE  rd.regimen           = 73                       
       		  AND  ad.ind_estado_cuenta = 0
       		  AND  tpo_prestacion = '00'
       		  AND  (   (tpo_seguro = 'IV' AND tpo_pension IN ('AS','OR','VI','VO','IN','CE','VE')) 
       		        OR (tpo_seguro = 'RT' AND tpo_pension IN ('AS','OR','VI','VO','IP'))          )
       		GROUP  BY rd.nss,ad.id_derechohabiente
         
        		--Se obtiene el id_datamart
        		SELECT MAX(id_datamart)
        		INTO   v_id_datamart
        		FROM   ret_datamart
        		WHERE  nss         = v_nss
        		AND    sec_pension = v_sec_pension;
        			
    			-- Se verifica si ha tenido retiro ley 73
				IF NOT EXISTS(SELECT id_derechohabiente
				              FROM ret_ley73
				              WHERE id_derechohabiente = v_id_derechohabiente) THEN

				    -- Se obtiene los saldos de vivienda 92 y 97			          
				    EXECUTE FUNCTION fn_saldo_dia(v_nss,NULL,8,TODAY) INTO v_resultado,v_saldo_aivs,v_saldo_pesos;
				    LET v_suma_vivienda_92_97 = v_saldo_aivs;
				    EXECUTE FUNCTION fn_saldo_dia(v_nss,NULL,4,TODAY) INTO v_resultado,v_saldo_aivs,v_saldo_pesos;
				    LET v_suma_vivienda_92_97 = v_suma_vivienda_92_97 + v_saldo_aivs;
	
				    -- Se valida que las suma de vivienda92 y vivienda97 sea mayor a 0
				    IF v_suma_vivienda_92_97 > 0 THEN
				
						-- Se verifica la convivencia de marca, para saber si tiene algun proceso en curso de Retiro Ley 73
				        EXECUTE FUNCTION fn_consulta_convivencia(v_nss,0,815) INTO v_rch_cod, v_rch_desc;
				
				        -- Si la marca convive, se valida que no se haya procesado el id_datamart
				        IF v_rch_cod = 0 THEN
				            LET v_ind_envio = NULL; -- Solicitud aceptada
				            LET v_num_notificaciones = v_num_notificaciones + 1;
				        ELSE
				            LET v_ind_envio    = 5; -- marca no convive
				        END IF -- v_rch_cod = 0
				
				    ELSE
				        LET v_ind_envio = 4; -- suma de saldos no es mayor a 0
				    END IF -- fn_v_suma_vivienda_92_97(v_nss) > 0
				
				ELSE
				    LET v_ind_envio = 3; -- Ya tuvo retiro
				END IF -- NOT(fn_tiene_retiro_ley73(v_id_derechohabiente))
    			                    
                INSERT INTO tmp_ret_notificacion(id_datamart       ,
                                             	 folio_notificacion,
                                             	 nss               ,
                                             	 id_derechohabiente,
                                             	 ind_envio         ,
                                             	 f_envio           ,
                                             	 folio_datamart    )
                                      	  VALUES(v_id_datamart       ,
                                          	     p_folio_notificacion,
                                                 v_nss               ,
                                                 v_id_derechohabiente,
                                                 v_ind_envio         ,
                                                 TODAY               ,
                                                 0                   );
       		  
	END FOREACH
	
	-- Se pasan los registros al historico
    INSERT INTO ret_notificacion
    SELECT * FROM tmp_ret_notificacion;
    
    RETURN v_num_notificaciones, r_isam_err, r_err_txt;

END FUNCTION;


