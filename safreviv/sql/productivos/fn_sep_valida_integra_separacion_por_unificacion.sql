






CREATE FUNCTION "safreviv".fn_sep_valida_integra_separacion_por_unificacion(p_usuario CHAR(20),p_id_derechohabiente DECIMAL(9,0),p_asociado CHAR(11),p_invadido CHAR(11))
RETURNING SMALLINT;

DEFINE v_sql_error  INTEGER;           
DEFINE v_isam_error SMALLINT;
DEFINE v_msg_error  CHAR(200);

DEFINE v_id_derechohabiente_asociado  DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificado DECIMAL(9,0);
DEFINE v_id_unificador				  DECIMAL(9,0);
DEFINE v_id_unificado                 DECIMAL(9,0);
DEFINE v_nss_unificado			 	  CHAR(11);
DEFINE v_marca_unificador             SMALLINT;
DEFINE v_marca_unificado 			  SMALLINT;
DEFINE v_num_familia                  INTEGER;
DEFINE v_respuesta                    SMALLINT;
DEFINE v_res_desmarca				  INTEGER;

   -- Captura el error sql
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
         --A cualquier error en los querys retorna 9, ya que no pertenecen a la misma familia
      LET v_respuesta = 9;
      RETURN v_respuesta;
   END EXCEPTION;
   
   LET v_respuesta = 1; -- MISMA FAMILIA SIN DESMARCA
   
--1. LOCALIZA EL INVADIDO Y ASOCIADO EN LOS REGISTROS DE UNIFICACION
   --Buscando Unificador a través del invadido, si no lo encuentra es error
      
   SELECT a.id_unificador
      INTO v_id_unificador
      FROM uni_det_unificador a
      WHERE  a.estado_familia = 1
      AND a.diagnostico    = 6
      AND a.id_derechohabiente = p_id_derechohabiente
      AND a.nss_unificador = p_invadido;
   IF DBINFO('SQLCA.SQLERRD2') = 0 THEN
	     LET v_respuesta = 9;
		 RETURN v_respuesta;
   END IF;
      --Validando marca 150
   SELECT marca 
      INTO v_marca_unificador
      FROM sfr_marca_activa
      WHERE id_derechohabiente = p_id_derechohabiente
      AND marca = 150;
	  
   --Buscando Unificado, que será el Asociado
   SELECT a.id_unificado,a.id_derechohabiente,b.marca,a.nsscta1
      INTO v_id_unificado,v_id_derechohabiente_unificado,
	       v_marca_unificado,v_nss_unificado
	  FROM uni_det_unificado a, OUTER sfr_marca_activa b
      WHERE a.id_derechohabiente = b.id_derechohabiente
      AND a.id_unificador = v_id_unificador
      AND a.id_unificado = b.n_referencia
	  AND a.estado_unificacion = 1
	  AND a.diagnostico = 6;
	  IF DBINFO('SQLCA.SQLERRD2') = 0 THEN
	     LET v_respuesta = 9;
		 RETURN v_respuesta;
	  END IF;

   IF p_asociado <> v_nss_unificado THEN
      LET v_respuesta = 9;
      RETURN v_respuesta;
   END IF;   
   --( Si ambos select se realizan sin errores entonces es de la misma familia y es Unificación simple
   --Adem�s el nss obtenido deber� ser igual al registrado en det_03 del archivo de entrada.

--2. HABILITAR NSS INVOLUCRADOS QUE SE ENCUENTREN INHABILITADOS

   IF v_marca_unificador == 150 THEN
      --Habilita NSS (COLOCA MARCA 110 y DESPUÉS DESMARCA)
      EXECUTE FUNCTION fn_desmarca_cuenta(p_id_derechohabiente,
                                    150,
                                    v_id_unificador,
                                    40,
                                    0,
                                    p_usuario,
                                    2204)
      INTO v_res_desmarca;
	  IF v_res_desmarca == 0 THEN
         LET v_respuesta = 10; -- RESPUESTA 10 MISMA FAMILIA CON DESMARCA DE CUENTAS
         --AÑADIR A REPORTE DETALLE DE CUENTAS REACTIVADAS 0 para invadido, 1 para asociado
	     --INSERT INTO tmp_sep_reactiva_uni VALUES (0,p_id_derechohabiente,p_id_derechohabiente,p_invadido,0);
	  END IF;
   END IF;
	
   IF v_marca_unificado == 150 THEN
      --Habilita NSS (COLOCA MARCA 110 y DESPUÉS DESMARCA)
      EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_unificado,
                                    150,
                                    v_id_unificado,
                                    40,
                                    0,
                                    p_usuario,
                                    2204)
      INTO v_res_desmarca;
	  IF v_res_desmarca == 0 THEN
         LET v_respuesta = 10; -- RESPUESTA 10 MISMA FAMILIA CON DESMARCA DE CUENTAS
         --AÑADIR A REPORTE DETALLE DE CUENTAS REACTIVADAS
	     --INSERT INTO tmp_sep_reactiva_uni VALUES (0,p_id_derechohabiente,v_id_derechohabiente_unificado,v_nss_unificado,1);
	  END IF;      
   END IF;

  --La función se lleva a cabo sin errores
  RETURN v_respuesta;

END FUNCTION;


