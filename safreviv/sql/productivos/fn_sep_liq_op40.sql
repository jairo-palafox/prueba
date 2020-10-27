






CREATE FUNCTION "safreviv".fn_sep_liq_op40(p_usuario     CHAR(20),
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
DEFINE v_id_op40                DECIMAL(9,0) ; 
DEFINE v_id_det_op27            DECIMAL(9,0) ;
DEFINE v_sql_error              INTEGER      ;
DEFINE v_ind_error              SMALLINT     ;
DEFINE v_id_derechohabiente_inv DECIMAL(9,0) ;
DEFINE v_id_derechohabiente_aso DECIMAL(9,0) ;
DEFINE v_ind_desmarca_inv       SMALLINT     ;
DEFINE v_ind_desmarca_aso       SMALLINT     ;
DEFINE v_ind_conciliar          SMALLINT     ;
DEFINE v_fecha_actual           DATE;
DEFINE v_n_referencia_marca     dec(10,0)    ;
DEFINE v_n_marca                smallint     ;

   LET v_ind_error = 0;

   -- se avanza maquinaria de control para el lote de op 40 que se liquido
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
      UPDATE sep_cza_op40
         SET estado = v_estado_destino
       WHERE folio  =  p_folio;
   END IF
   
   FOREACH SELECT det2.id_det_02_op40,
                  det2.invadido      ,
                  det2.id_derechohabiente_invadido,
                  det3.asociado      ,
                  det3.id_derechohabiente_asociado,
                  det2.ind_desmarca     ,
                  det3.ind_desmarca     ,
                  det2.ind_conciliar ,
                  det2.estado
             INTO v_id_op40          ,
                  v_invadido         ,
                  v_id_derechohabiente_inv,
                  v_asociado         ,
                  v_id_derechohabiente_aso,
                  v_ind_desmarca_inv ,
                  v_ind_desmarca_aso ,
                  v_ind_conciliar    ,
                  v_estado  
             FROM sep_det_02_op40 det2 JOIN sep_det_03_op40 det3
                  ON det3.id_det_02_op40 = det2.id_det_02_op40
            WHERE det2.folio = p_folio
            AND   det2.estado = 15 --preliquidado
            AND   det2.ind_conciliar = 1 --aceptadas
   
            LET v_fecha_actual = TODAY;
    
             
            UPDATE sep_det_02_op40 
            SET    estado = 35 --liquidado
            WHERE  id_det_02_op40 = v_id_op40;

 
 
      IF v_ind_conciliar = 1 THEN -- Preliquidado sin diferencia en saldos
            IF  v_ind_desmarca_inv = 1 THEN
                -- Desmarca cuenta invadida sólo si se ha preliquidado sin diferencias en saldos y
                -- es informado que se debe desmarcar la cuenta (v_ind_marca = 1)

                LET v_n_referencia_marca = 0;
                LET v_n_marca = 0;
                FOREACH SELECT a.n_referencia ,a.marca
                    INTO   v_n_referencia_marca, v_n_marca
                    FROM   sfr_marca_activa a
                    WHERE  a.id_derechohabiente = v_id_derechohabiente_inv
                    AND    a.marca in (280,551)


                    EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_inv ,
                                                        v_n_marca                    , -- (Marca) Separación de cuentas
                                                        v_n_referencia_marca     , -- (Referencia)
                                                        100                      , -- (Estado marca "Finalizada")
                                                        280                      , -- (Marca causa) Separación de cuentas
                                                        p_usuario                ,
                                                        p_proceso_cod  )
                               INTO v_ind_error;
                END FOREACH;
           END IF

           IF v_ind_desmarca_aso = 1 THEN
                LET v_n_referencia_marca = 0;
                LET v_n_marca = 0;
                FOREACH SELECT a.n_referencia ,a.marca
                    INTO   v_n_referencia_marca, v_n_marca
                    FROM   sfr_marca_activa a
                    WHERE  a.id_derechohabiente = v_id_derechohabiente_aso
                    AND    a.marca in (702,551)
 
                    EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_aso ,
                                                        v_n_marca                , -- (Marca) Separación de cuentas
                                                        v_n_referencia_marca     , -- (Referencia)
                                                        100                      , -- (Estado marca Finalizada)
                                                        280                      , -- (Marca causa) Separación de cuentas
                                                        p_usuario                ,
                                                        p_proceso_cod )
                                        INTO v_ind_error;
   
                END FOREACH;
           END IF;
      END IF;

   END FOREACH;
   RETURN v_ind_error;

END FUNCTION;


