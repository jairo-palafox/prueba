






CREATE FUNCTION "safreviv".fn_sep_liq_op28(p_usuario     CHAR(20),
                                p_pid         INTEGER ,
                                p_proceso_cod SMALLINT , 
                                p_opera_cod   SMALLINT ,
                                p_folio       DECIMAL(9,0))
RETURNING SMALLINT;                                         

DEFINE v_invadido               CHAR(011)    ;
DEFINE v_diag                   CHAR(003)    ;
DEFINE v_asociado               CHAR(011)    ;
DEFINE v_estado                 SMALLINT     ;
DEFINE v_ind                    SMALLINT     ;
DEFINE v_estado_destino         SMALLINT     ;
DEFINE v_id_expediente          DECIMAL(9,0) ;
DEFINE v_id_op28                DECIMAL(9,0) ; 
DEFINE v_id_det_op27            DECIMAL(9,0) ;
DEFINE v_sql_error              INTEGER      ;
DEFINE v_ind_error              SMALLINT     ;
DEFINE v_id_derechohabiente_inv DECIMAL(9,0) ;
DEFINE v_ind_marca_inv          SMALLINT     ;
DEFINE v_ind_marca_asc          SMALLINT     ;
DEFINE v_ind_conciliar          SMALLINT     ;
DEFINE v_fecha_actual           DATE;
DEFINE v_id_derechohabiente_asc DECIMAL(9,0);
DEFINE v_id_det_03_op27         DECIMAL(9,0);

   LET v_ind_error = 0;

   -- se avanza maquinaria de control para el lote de op 28 que se liquido
   -- senal 30 "liquidar" y estado origen 20 "Preliquidado"
   -- se actualiza la cza primero para facilitar el reverso de la operación
   
   EXECUTE FUNCTION fn_maquinaria("maq_sep_ctr_op28",
                                  30, -- (Señal) liquidar
                                  20) -- (Estado) Preliquidado
                             INTO v_ind, 
                                  v_diag,
                                  v_estado_destino;
   
   IF v_ind <> 0 THEN 
      LET v_ind_error = -5;   
   ELSE
      UPDATE sep_cza_op28
         SET estado = v_estado_destino
       WHERE folio  =  p_folio;
   END IF
   
   FOREACH SELECT det2.id_det_02_op28,
                  det2.invadido      ,
                  det2.id_derechohabiente_invadido,
                  det3.asociado      ,
                  det2.id_det_op27   ,
                  det2.ind_marca     ,
				  det3.ind_marca     ,
                  det2.ind_conciliar ,
                  det2.estado		 ,
				  det3.id_derechohabiente_asociado
             INTO v_id_op28          ,
                  v_invadido         ,
                  v_id_derechohabiente_inv,
                  v_asociado         ,
                  v_id_det_op27      ,
                  v_ind_marca_inv    ,
				  v_ind_marca_asc    ,
                  v_ind_conciliar    ,
                  v_estado  		 ,
                  v_id_derechohabiente_asc
			 FROM sep_det_02_op28 det2 JOIN sep_det_03_op28 det3
                  ON det3.id_det_02_op28 = det2.id_det_02_op28
            WHERE det2.folio = p_folio
   
      LET v_fecha_actual = TODAY;
      
      EXECUTE FUNCTION sp_sep_consulta_expediente(v_invadido,
                                                  v_asociado,
                                                  40) -- DICTAMEN REGISTRADO
                                             INTO v_ind          , 
                                                  v_id_expediente,   
                                                  v_sql_error    ;
   
      IF v_ind <> 1 THEN 
         EXECUTE FUNCTION sp_sep_consulta_expediente(v_invadido,
                                                     v_asociado,
                                                     45) -- RESTITUCION SOLICITADA
                                                INTO v_ind          , 
                                                     v_id_expediente,   
                                                     v_sql_error    ;
                                                 
         IF v_ind <> 1 THEN 
            EXECUTE FUNCTION sp_sep_consulta_expediente(v_invadido,
                                                        v_asociado,
                                                        46) 
                                                   INTO v_ind            , 
                                                        v_id_expediente  ,   
                                                        v_sql_error      ;
   
         END IF
      END IF
   
   
      -- si se encuentra el expediente se avanza expediente y op28
      IF( v_ind = 1 )THEN
   
         --Ejecuta avance de maquinaria de expediente
         EXECUTE FUNCTION fn_maquinaria_individual("maq_sep_expediente",
                                                   v_id_expediente    ,
                                                   "id_expediente",
                                                   60,
                                                   "p_usuario")
                                              INTO v_ind,
                                                   v_diag,
                                                   v_estado_destino;
   
         IF (v_ind <> 0) THEN 
              LET v_ind_error = -1;
         END IF 
         -- Ejecut avance de maquinaria de op 28
         EXECUTE FUNCTION fn_maquinaria_individual("maq_sep_det_op28"  ,
                                                   v_id_op28          ,
                                                   "id_det_02_op28"    ,
                                                   35,
                                                   "p_usuario")
                                              INTO v_ind,
                                                   v_diag,
                                                   v_estado_destino;
   
         IF( v_ind <> 0 )THEN 
            LET v_ind_error = -2;
         END IF
      ELSE 
         -- si no se encuentra expediente se avanza solo op28
         IF( v_ind= 0 )THEN
            -- Ejecut avance de maquinaria de op 28
            EXECUTE FUNCTION fn_maquinaria_individual("maq_sep_det_op28"  ,
                                                      v_id_op28          ,
                                                      "id_det_02_op28"    ,
                                                      35,
                                                      "p_usuario")
                                                 INTO v_ind,
                                                      v_diag,
                                                      v_estado_destino;
   
            IF( v_ind <> 0 )THEN 
               LET v_ind_error = -3;
            END IF 
         ELSE  
            LET v_ind_error = -4;
         END IF
      END IF
      IF(v_ind_conciliar = 6 OR v_ind_conciliar = 9 )THEN -- 6 - Preliquidado sin diferencia en saldos / 9 - Segunda vuelta para desmarca
	     --Obteniendo id_det_03_op27 para realizar desmarca
		 SELECT id_det_03_op27
            INTO v_id_det_03_op27
			FROM sep_det_02_op27 a, sep_det_03_op27 b
            WHERE a.id_det_02_op27 = b.id_det_02_op27
            AND a.id_det_02_op27 = v_id_det_op27
			AND b.id_derechohabiente_asociado = v_id_derechohabiente_asc;

         IF v_ind_marca_inv = 1 THEN  -- DESMARCA DE MANERA INDIVIDUAL INVADIDO 280 y ASOCIADO 702
            -- Desmarca cuenta invadida sólo si se ha preliquidado sin diferencias en saldos y es informado que se debe desmarcar la cuenta (v_ind_marca = 1) 
            EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_inv,
                                                280, -- (Marca) Separación de cuentas
                                                v_id_det_op27, -- (Referencia)
                                                0,   -- (Estado marca)
                                                280, -- (Marca causa) Separación de cuentas
                                                p_usuario,
                                                p_proceso_cod)
                                           INTO v_ind_error;
         END IF;
		 IF v_ind_marca_asc = 1 THEN
			EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_asc,
                                                702, -- (Marca) Separación de cuentas
                                                v_id_det_03_op27, -- (Referencia)
                                                0,   -- (Estado marca)
                                                702, -- (Marca causa) Separación de cuentas
                                                p_usuario,
                                                p_proceso_cod)
                                           INTO v_ind_error;
		 END IF;
      ELSE -- el resto donde v_ind_conciliar = 3 ó 4 ó 5 ó 7 ó 8
         -- 3 preliquidado viv92, diferencias en viv97
         -- 4 preliquidado viv97, diferencias en viv92
         -- 5 Con diferencias en viv92 y viv97
         -- 7 Saldos diferentes al primer día natural del mes
         -- 8 Saldos diferentes al día actual de preliquidación
         -- Establece marca de Aclaración
         EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_inv,
                                          551, -- (Marca) Aclaración separacion
                                          v_id_op28, -- (Referencia) id_det02_op28
                                          p_folio,
                                          0, -- estado de marca
                                          0, -- Cod rechazo
                                          0, -- (Marca causa) Separación de invadido
                                          v_fecha_actual, -- Fecha causa
                                          p_usuario,
                                          p_proceso_cod)
                                     INTO v_ind_error;
      END IF
   END FOREACH;

   RETURN v_ind_error;

END FUNCTION;


