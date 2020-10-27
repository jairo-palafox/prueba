






CREATE PROCEDURE "safreviv".sp_hps_valida_catalogo_mdt()
--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 1 de julio de 2015
-- Realiza la validación del archivo cargado correspondiente al layout 
-- De actualizacion de catálogo mandatos,layout_cod 3102
--===============================================================
RETURNING INTEGER,
          INTEGER,
          CHAR(254);

--Definiendo variables de salida		  
DEFINE v_error_sql  INTEGER;
DEFINE v_isam_error INTEGER;
DEFINE v_msg_error  CHAR(254);

--Definiendo variales internas
DEFINE v_tipo_registro			DECIMAL(1,0);
DEFINE v_contador_servicio   	DECIMAL(6,0);
DEFINE v_tipo_mandato        	DECIMAL(1,0);
DEFINE v_entidad_federativa 	CHAR(2);
DEFINE v_municipio           	CHAR(5);
DEFINE v_nombre_desarrollo   	CHAR(120);
DEFINE v_desarrollador       	CHAR(120);
DEFINE v_numero_cuenta       	DECIMAL(10,0);
DEFINE v_administrador       	CHAR(120);
DEFINE v_rfc                 	CHAR(13);
DEFINE v_numero_acreedor     	DECIMAL(10,0);
DEFINE v_promotor_vecinal		CHAR(120);
DEFINE v_resultado_opera		CHAR(2);
DEFINE v_diagnostico			CHAR(3);

DEFINE v_mensaje	 			CHAR(255);
DEFINE eval_consecutivo         DECIMAL(6,0);
DEFINE eval_entidad_fed			SMALLINT;
DEFINE eval_municipio			SMALLINT;
DEFINE eval_det02				SMALLINT;
DEFINE eval_paquete 			SMALLINT;
DEFINE eval_desarrollo			SMALLINT;
DEFINE eval_desarrollador		SMALLINT;
DEFINE eval_administrador       SMALLINT;
DEFINE eval_rfc                 SMALLINT;
DEFINE eval_promotor			SMALLINT;

--retorna datos segun excepcion
ON EXCEPTION SET 	v_error_sql,
                    v_isam_error,
                    v_msg_error
   
   RETURN v_error_sql,
		  v_isam_error,
		  v_msg_error;

END EXCEPTION

--INICIA la función, definiendo valores iniciales
   LET v_error_sql  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = "Consulta realizada correctamente";

--VALIDANDO DETALLE 01
   FOREACH cur_det01 FOR  SELECT * INTO 	
							v_tipo_registro,
							v_contador_servicio,
							v_tipo_mandato,
							v_entidad_federativa,
							v_municipio,
							v_nombre_desarrollo,
							v_desarrollador,
							v_numero_cuenta,
							v_administrador,
							v_rfc,
							v_numero_acreedor,
							v_promotor_vecinal,
							v_resultado_opera,
							v_diagnostico
						FROM safre_tmp:hps_tmp_det_acmdt
						WHERE tipo_mandato = 1 OR tipo_mandato = 2
		
       --Valores iniciales		
	   LET v_resultado_opera = "01";
	   LET v_diagnostico     = "000";
	   LET eval_desarrollo 		= LENGTH(v_nombre_desarrollo);
	   LET eval_desarrollador 	= LENGTH(v_desarrollador);
	   LET eval_administrador 	= LENGTH(v_administrador);
	   LET eval_rfc	 			= LENGTH(v_rfc);
	   LET eval_promotor 		= LENGTH(v_promotor_vecinal);
	   	   
	   --Validar que existe municipio
	   SELECT COUNT(*)
	      INTO eval_municipio
		  FROM safre_viv:cat_municipio
		  WHERE municipio = v_municipio::SMALLINT
		  AND entidad_federativa = v_entidad_federativa::SMALLINT;
	   IF eval_municipio < 1 THEN
	      LET v_resultado_opera = "02";
	      LET v_diagnostico     = "002"; --NO EXISTE MUNICIPIO O NO PERTENECE A ENTIDAD FEDERATIVA
		  LET v_mensaje ="Registro invalido: "||v_tipo_mandato||" "||v_contador_servicio||" municipio "||v_municipio||" entidad_federativa "||v_entidad_federativa;
	   END IF;
	   
	   --Validando si existe entidad_federativa
	   SELECT COUNT(*)
	      INTO eval_entidad_fed
	      FROM safre_viv:cat_entidad_federativa 
	   	  WHERE entidad_federativa = v_entidad_federativa::SMALLINT;
	   IF eval_entidad_fed < 1 THEN
	      LET v_resultado_opera = "02";
	      LET v_diagnostico     = "001"; --NO EXISTE ENTIDAD FEDERATIVA
		  LET v_mensaje ="Registro invalido: "||v_tipo_mandato||" "||v_contador_servicio||" entidad_federativa "||v_entidad_federativa;
	   END IF;
	  
	  --PARA CUOTA DE CONSERVACION
	   IF v_tipo_mandato == 2 THEN
	      
		  IF v_nombre_desarrollo IS NULL OR eval_desarrollo == 0 THEN
		     LET v_resultado_opera = "02";
	         LET v_diagnostico     = "003"; --nombre desarrollo es nulo
		     LET v_mensaje ="Registro invalido: "||v_tipo_mandato||" "||v_contador_servicio||" Desarrollo no debe ser nulo para Cuota de Conservacion";
		  END IF;
		  
		  IF v_desarrollador IS NULL OR eval_desarrollador == 0 THEN
		     LET v_resultado_opera = "02";
	         LET v_diagnostico     = "004"; --nombre desarrollador es nulo
		     LET v_mensaje ="Registro invalido: "||v_tipo_mandato||" "||v_contador_servicio||" Desarrollador no debe ser nulo para Cuota de Conservacion";
		  END IF;

		  IF v_numero_cuenta IS NULL THEN
		     LET v_resultado_opera = "02";
	         LET v_diagnostico     = "005"; --numero de cuenta es nulo
		     LET v_mensaje ="Registro invalido: "||v_tipo_mandato||" "||v_contador_servicio||" Numero de Cuenta no debe ser nulo para Cuota de Conservacion";
		  END IF;
		  
		  IF v_administrador IS NULL OR eval_administrador == 0 THEN
		     LET v_resultado_opera = "02";
	         LET v_diagnostico     = "006"; --administrador es nulo
		     LET v_mensaje ="Registro invalido: "||v_tipo_mandato||" "||v_contador_servicio||" Administrador no debe ser nulo para Cuota de Conservacion";
		  END IF;
		  
		  IF v_rfc IS NULL OR eval_rfc == 0 THEN
		     LET v_resultado_opera = "02";
	         LET v_diagnostico     = "007"; --rfc es nulo
		     LET v_mensaje ="Registro invalido: "||v_tipo_mandato||" "||v_contador_servicio||" RFC Administrador no debe ser nulo para Cuota de Conservacion";
		  END IF;
		  
		  IF v_numero_acreedor IS NULL THEN
		     LET v_resultado_opera = "02";
	         LET v_diagnostico     = "008"; --numero acreedor es nulo
		     LET v_mensaje ="Registro invalido: "||v_tipo_mandato||" "||v_contador_servicio||" Numero de Acreedor no debe ser nulo para Cuota de Conservacion";
		  END IF;
		  
		  IF v_promotor_vecinal IS NULL OR eval_promotor == 0 THEN
		     LET v_resultado_opera = "02";
	         LET v_diagnostico     = "009"; --Promotor vecinal es nulo
		     LET v_mensaje ="Registro invalido: "||v_tipo_mandato||" "||v_contador_servicio||" Promotor Vecinal no debe ser nulo para Cuota de Conservacion";
		  END IF;
		  
		  SELECT COUNT(*)
		     INTO eval_det02
			 FROM safre_tmp:hps_tmp_det02_acmdt
			 WHERE contador_servicio = v_contador_servicio; 
	      IF eval_det02 < 1 THEN
	         LET v_resultado_opera = "02";
	         LET v_diagnostico     = "010"; --NO EXISTE Correspondiente en det02
		     LET v_mensaje ="Registro invalido: "||v_tipo_mandato||" "||v_contador_servicio||" Cuota de Conservacion debe contar con identificador de paquete";
	      ELSE 
		     SELECT COUNT(*) 
		        INTO eval_paquete
		        FROM safre_tmp:hps_tmp_det02_acmdt
			    WHERE contador_servicio = v_contador_servicio
				AND paquete MATCHES "02[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]";
			 IF eval_paquete < 1 THEN
			    LET v_resultado_opera = "02";
	            LET v_diagnostico     = "011"; --PAQUETE INCORRECTO
		        LET v_mensaje ="Registro invalido: "||v_tipo_mandato||" "||v_contador_servicio||" Estructura de paquete invalida";
			 END IF;
		  END IF;
		  
	   END IF;
	   
	   --PARA PREDIAL
	   IF v_tipo_mandato == 1 THEN
	   
	      IF (v_nombre_desarrollo IS NULL OR eval_desarrollo == 0)
		     AND (v_desarrollador IS NULL OR eval_desarrollador == 0)
			 AND (v_numero_cuenta IS NULL)
			 AND (v_administrador IS NULL OR eval_administrador == 0)
			 AND (v_rfc           IS NULL OR eval_rfc == 0 )
			 AND (v_numero_acreedor IS NULL)
			 AND (v_promotor_vecinal IS NULL OR eval_promotor == 0 )THEN
			 --DO NOTHING
		  ELSE
             LET v_resultado_opera = "02";
	         LET v_diagnostico     = "012"; --Datos no nulos para predial
		     LET v_mensaje ="Registro invalido: "||v_tipo_mandato||" "||v_contador_servicio||" Datos invalidos para tipo Predial";
		  END IF;
	   
	   END IF;
	  
	   --Validando consecutivo
	   IF v_contador_servicio IS NULL THEN
	      LET v_resultado_opera = "02";
	      LET v_diagnostico     = "000"; --ERROR DE NULOS
		  LET v_mensaje ="Registro invalido: "||v_tipo_mandato||" Contador_servicio es nulo";
	   END IF;
	   
	   --TERMINAN VALIDACIONES
	   --/////////////////////////////////////////////////////////////////////////////////
	    --AGREGAR A LA TABLA TEMPORAL EL ERROR
	   IF v_resultado_opera != "01" THEN
		  INSERT INTO safre_viv:hps_diag_tmp_inconsistencias_mdt VALUES (v_mensaje); 
	   END IF;
	   
	   --ACTUALIZANDO EL REGISTRO
	   UPDATE safre_tmp:hps_tmp_det_acmdt 
	      SET resultado_opera = v_resultado_opera,
		      diagnostico 	  = v_diagnostico
		  WHERE CURRENT OF cur_det01;
		    
   END FOREACH
   
--FINALIZACION, retorna valores
   RETURN v_error_sql,
		  v_isam_error,
		  v_msg_error;

END PROCEDURE;


