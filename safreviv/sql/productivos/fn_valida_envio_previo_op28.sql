






CREATE FUNCTION "safreviv".fn_valida_envio_previo_op28(p_nss_inv       CHAR(11), -- NSS Invadido de Op28
                                            p_ind_marca_inv CHAR(1),  -- ind_marca detalle 02
											p_nss_asc       CHAR(11), -- NSS Asociado de Op28
											p_ind_marca_asc CHAR(1))  -- ind_marca detalle 03
											
RETURNING SMALLINT       , -- (0 si no existe envío previo o no cumple con todas las condiciones / 1 Si existe envío previo y cumple con todas las condiciones
          SMALLINT       ,
          INTEGER        , -- sql_error
          INTEGER        , -- isam error
          VARCHAR(250);

--control		  
DEFINE v_ind              SMALLINT; 
DEFINE v_diag             CHAR(3);
DEFINE resultado_valida   SMALLINT;
DEFINE f_ind_conciliar    SMALLINT;
DEFINE v_sql_error        INTEGER; 
DEFINE isam_err           INTEGER;
DEFINE err_txt            VARCHAR(255);

--variables sp
DEFINE v_id_det_02_op28   DECIMAL(9,0);
DEFINE v_id_det_02_op27   DECIMAL(9,0);
DEFINE v_estado           SMALLINT;
DEFINE v_ant_marca_inv    CHAR(1);
DEFINE v_ant_marca_asc    CHAR(1);
DEFINE v_ref_marca_280    DECIMAL(9,0);  
DEFINE v_ref_marca_702    DECIMAL(9,0);
DEFINE v_marca            SMALLINT;
DEFINE v_id_derecho_inv   DECIMAL(9,0);
DEFINE v_id_derecho_asc   DECIMAL(9,0);
DEFINE v_ind_conciliar    DECIMAL(9,0);

   -- se establece el comportamiento ante una aparcicion de error
   ON EXCEPTION SET v_sql_error,isam_err, err_txt
   
      -- se indica que hubo error
      LET resultado_valida    = 0      ;

   RETURN resultado_valida,
          f_ind_conciliar,
          v_sql_error  ,
          isam_err     ,
          err_txt      ;

   END EXCEPTION
   
--INICIALIZA VARIABLES

LET resultado_valida = 0;
LET f_ind_conciliar  = 0;
LET v_sql_error      = 0;
LET isam_err         = 0;
LET err_txt          = "No existen registros de envío previo";

   --VALIDA QUE EXISTA ENVÍO PREVIO
   FOREACH SELECT FIRST 1 a.id_det_02_op28,
   		          a.id_det_op27,
                  a.estado,
		          a.ind_marca,
		          b.ind_marca,
		          a.id_derechohabiente_invadido,
		          b.id_derechohabiente_asociado,
				  a.ind_conciliar
      INTO v_id_det_02_op28,
	       v_id_det_02_op27,
	       v_estado,
	       v_ant_marca_inv,
	       v_ant_marca_asc,
	       v_id_derecho_inv,
	       v_id_derecho_asc,
		   v_ind_conciliar
	  FROM sep_det_02_op28 a,sep_det_03_op28 b
	  WHERE a.id_det_02_op28 = b.id_det_02_op28
	  AND a.invadido = p_nss_inv
	  AND b.asociado = p_nss_asc
	  ORDER BY a.f_proceso DESC
	  
	    -- VALIDA DATOS DE ENTRADA
		IF p_ind_marca_inv <> " " AND p_ind_marca_inv <> 1 THEN
		  LET err_txt = "Validar datos de entrada del invadido";
		  RETURN resultado_valida,
		         f_ind_conciliar,
				 v_sql_error,
				 isam_err,
				 err_txt;
		END IF;
		IF p_ind_marca_asc <> " " AND p_ind_marca_asc <> 1 THEN
		  LET err_txt = "Validar datos de entrada del asociado";
		  RETURN resultado_valida,
		         f_ind_conciliar,
				 v_sql_error,
				 isam_err,
				 err_txt;	
		END IF;

		-- VALIDA QUE SE ENCUENTRE EN ESTADO 25 LIQUIDADO
		IF v_estado <> 25 THEN
		  LET err_txt = "El envío previo no se encuentra en estatus liquidado";
		  RETURN resultado_valida,
		         f_ind_conciliar,
				 v_sql_error,
				 isam_err,
				 err_txt;
		END IF;

		-- OBTIENE REFERENCIAS DE MARCAS 
		LET v_ref_marca_280 = v_id_det_02_op27;
		SELECT id_det_03_op27
		  INTO v_ref_marca_702
		  FROM sep_det_03_op27
		  WHERE id_det_02_op27 = v_id_det_02_op27;
		IF SQLCODE = 100 THEN
		  LET err_txt = "Existe un problema al obtener las parejas de Op 27";
		  RETURN resultado_valida,
		         f_ind_conciliar,
				 v_sql_error,
				 isam_err,
				 err_txt;      
		END IF;

		-- INVADIDO DESMARCADO PROCESO PREVIO
		IF p_ind_marca_inv = ' ' THEN
		  IF v_ant_marca_inv = 1 THEN	  
		     LET v_marca = 0;
			 SELECT COUNT(*)
				INTO v_marca
				FROM sfr_marca_historica
				WHERE marca = 280
				AND id_derechohabiente = v_id_derecho_inv
				AND n_referencia = v_ref_marca_280
				AND f_fin IS NOT NULL;
			 IF v_marca = 0 THEN -- busca marca historica 280 concluida
				LET err_txt = "La cuenta invadido no se desmarcó en el proceso previo";
				RETURN resultado_valida,
				       f_ind_conciliar,
					   v_sql_error,
					   isam_err,
					   err_txt;
			 END IF;
		  ELSE
			 LET err_txt = "El indicador de marca del envío previo para el invadido debe estar como 1 - desmarca cuenta";
				RETURN resultado_valida,
				       f_ind_conciliar,
					   v_sql_error,
					   isam_err,
					   err_txt;
		  END IF;
		END IF;

		-- INVADIDO DESMARCAR
		IF p_ind_marca_inv = 1 THEN
		  IF v_ant_marca_inv = 0 THEN
		     LET v_marca = 0;
			 SELECT COUNT(*)
				INTO v_marca
				FROM sfr_marca_activa
				WHERE marca = 280
				AND id_derechohabiente = v_id_derecho_inv
				AND n_referencia = v_ref_marca_280;
			 IF v_marca = 0 THEN -- busca marca historica 280 concluida
				LET err_txt = "La cuenta invadido no sigue marcada";
				RETURN resultado_valida,
				       f_ind_conciliar,
					   v_sql_error,
					   isam_err,
					   err_txt;
			 END IF;
		  ELSE
			 LET err_txt = "El indicador desmarca del envío previo para el invadido debe estar como 0 - continua marcado ";
				RETURN resultado_valida,
				       f_ind_conciliar,
					   v_sql_error,
					   isam_err,
					   err_txt;
		  END IF;
		END IF;

		-- ASOCIADO DESMARCADO PROCESO PREVIO
		IF p_ind_marca_asc = ' ' THEN
		  IF v_ant_marca_asc = 1 THEN	  
		     LET v_marca = 0 ;
			 SELECT COUNT(*)
				INTO v_marca
				FROM sfr_marca_historica
				WHERE marca = 702
				AND id_derechohabiente = v_id_derecho_asc
				AND n_referencia = v_ref_marca_702
				AND f_fin IS NOT NULL;
			 IF v_marca = 0 THEN -- busca marca historica 702 concluida
			--	LET err_txt = "La cuenta asociado no se desmarcó en el proceso previo";
			--	RETURN resultado_valida,
			--	       f_ind_conciliar,
			--		   v_sql_error,
			--		   isam_err,
			--		   err_txt;
			 END IF;
		  ELSE
			 LET err_txt = "El indicador de marca del envío previo del asociado debe estar como 1 - desmarca cuenta";
				RETURN resultado_valida,
				       f_ind_conciliar,
					   v_sql_error,
					   isam_err,
					   err_txt;
		  END IF;
		END IF;

		-- ASOCIADO DESMARCAR
		IF p_ind_marca_asc = 1 THEN
		  IF v_ant_marca_asc = 0 THEN	  
		     LET v_marca = 0 ;
			 SELECT COUNT(*)
				INTO v_marca
				FROM sfr_marca_activa
				WHERE marca = 702
				AND id_derechohabiente = v_id_derecho_asc
				AND n_referencia = v_ref_marca_702;
			 IF v_marca = 0 THEN -- busca marca historica 280 concluida
				LET err_txt = "La cuenta asociado no sigue marcada";
				RETURN resultado_valida,
				       f_ind_conciliar,
					   v_sql_error,
					   isam_err,
					   err_txt;
			 END IF;
		  ELSE
			 LET err_txt = "El indicador de marca del envío previo del asociado debe estar como 0 - continua marcado ";
				RETURN resultado_valida,
				       f_ind_conciliar,
					   v_sql_error,
					   isam_err,
					   err_txt;
		  END IF;
		END IF;
		
      --SI CUMPLE CON TODAS LAS VALIDACIONES EL RESULTADO ES CORRECTO 
      LET resultado_valida = 1 ;
      LET err_txt = "OK";
	  IF v_ind_conciliar = 6 THEN
	     LET f_ind_conciliar = 9; --CON ENVÍO PREVIO SIN DIFERENCIAS
	  ELSE
         LET f_ind_conciliar = 10; -- CON ENVÍO PREVIO, PERO CON DIFERENCIAS
	  END IF;
	  
   END FOREACH;
	  
   -- se devuelve el resultado de la ejecucion
   RETURN resultado_valida,
          f_ind_conciliar,
          v_sql_error  ,
          isam_err     ,
          err_txt      ;
	   
END FUNCTION;


