






CREATE FUNCTION "safreviv".fn_cred_iniciativa(p_nss CHAR(11))
   RETURNING SMALLINT,DECIMAL(10,0),DECIMAL(12,2),DECIMAL(12,2),SMALLINT,DATE,CHAR(15),DATE

   DEFINE v_f_liq_cred       DATE;   
   DEFINE v_qry              CHAR(500);
   DEFINE v_error            SMALLINT;
   DEFINE v_num_credito      DECIMAL(10,0);
   DEFINE v_aivs             DECIMAL(12,2);
   DEFINE v_pesos            DECIMAL(12,2);
   DEFINE v_tpo_credito      SMALLINT;
   DEFINE v_f_cargo          DATE;
   DEFINE v_estatus          CHAR(15);
   DEFINE v_f_liquida        DATE;
   DEFINE v_nss              CHAR(11);
   DEFINE v_id_dh            DECIMAL(9,0);
   DEFINE v_id_acreditado    DECIMAL(9,0);
   DEFINE v_estado           SMALLINT;
   DEFINE v_edo_procesar     SMALLINT;
   DEFINE v_folio_liquida    DECIMAL(9,0);
   DEFINE v_entidad          SMALLINT;
   DEFINE v_f_otorga         DATE;
   DEFINE v_id_deudor        DECIMAL(9,0);
   DEFINE v_tabla            CHAR(20);
   DEFINE bnd_sin_datos      SMALLINT;
   DEFINE v_resultado        SMALLINT;
   DEFINE v_tpo_orig         SMALLINT;
   DEFINE v_tpo_cred         SMALLINT;
   DEFINE v_num_cred         DECIMAL(10,0);
   DEFINE v_f_otor           DATE;
   DEFINE v_tpo_dscto        SMALLINT;
   DEFINE v_cnt_reg          INTEGER;
   DEFINE v_tpo_originacion  smallint;

   define v_nom              char(50);
   define v_variable         smallint;
   define v_cnt_monto        integer;

   ON EXCEPTION SET v_error
      LET v_num_credito = NULL ;
      LET v_aivs        = NULL ;
      LET v_pesos       = NULL ;
      LET v_tpo_credito = NULL ;
      LET v_f_cargo     = NULL ;
      LET v_estatus     = NULL ;
      LET v_f_liquida   = NULL ;
      LET v_f_liq_cred  = NULL ;
      RETURN v_error,v_num_credito,v_aivs,v_pesos,v_tpo_credito,v_f_cargo,v_estatus,v_f_liq_cred;
   END EXCEPTION;

   ---SET DEBUG FILE TO '/safreviv_int/archivos/fn_cred_iniciativa.trace';
   ---TRACE ON;

   LET v_error           = 0;
   LET v_num_credito     = NULL ;
   LET v_aivs            = NULL ;
   LET v_pesos           = NULL ;
   LET v_tpo_credito     = NULL ;
   LET v_f_cargo         = NULL ;
   LET v_estatus         = NULL ;
   LET v_f_liquida       = NULL ;
   LET v_nss             = p_nss;
   LET v_id_dh           = NULL ;
   LET v_id_acreditado   = NULL ;
   LET v_estado          = NULL ;
   LET v_edo_procesar    = NULL ;
   LET v_tpo_credito     = NULL ;
   LET v_folio_liquida   = NULL ;
   LET v_entidad         = NULL ;
   LET v_f_otorga        = NULL ;
   LET v_id_deudor       = NULL ;
   LET v_tabla           = NULL ;
   LET bnd_sin_datos     = 0    ;
   LET v_cnt_reg         = 0    ;
   LET v_f_liq_cred      = NULL ;
   LET v_tpo_originacion = NULL ;

   LET v_resultado       = NULL ;
   LET v_tpo_orig        = NULL ;
   LET v_tpo_cred        = NULL ;
   LET v_num_cred        = NULL ;
   LET v_f_otor          = NULL ;
   LET v_tpo_dscto       = NULL ;

   SELECT id_derechohabiente
     INTO v_id_dh
     FROM afi_derechohabiente
    WHERE nss = v_nss;

   SELECT COUNT(*)
     INTO v_cnt_reg
     FROM cre_acreditado c,
          cat_maq_credito m,
    outer cat_tipo_credito t
    WHERE id_derechohabiente = v_id_dh
      AND c.estado = m.estado
      AND m.entidad in(1,2,5)
      AND c.tpo_originacion = t.tpo_originacion
      AND c.tpo_credito = t.tpo_credito;

   IF v_cnt_reg >= 1 THEN
      FOREACH
      SELECT FIRST 1 c.id_cre_acreditado,
             c.estado,
             c.edo_procesar,
             c.tpo_credito,
             c.num_credito,
             c.folio_liquida,
             m.entidad,
             c.f_otorga,
             t.id_deudor,
             c.tpo_originacion
        INTO v_id_acreditado,
             v_estado,
             v_edo_procesar,
             v_tpo_credito,
             v_num_credito,
             v_folio_liquida,
             v_entidad,
             v_f_otorga,
             v_id_deudor,
             v_tpo_originacion
        FROM cre_acreditado c,
             cat_maq_credito m,
       outer cat_tipo_credito t
       WHERE id_derechohabiente = v_id_dh
         AND c.estado = m.estado
         AND m.entidad in(1,2,5)
         AND c.tpo_originacion = t.tpo_originacion
         AND c.tpo_credito = t.tpo_credito
       order by c.f_otorga ,c.estado desc, c.folio_liquida desc
      END FOREACH;
   END IF

   IF (v_id_deudor IS NULL) OR
      (v_id_deudor = 0) THEN
      LET bnd_sin_datos = 1;
   ELSE
      IF (v_id_deudor = 1) AND
         (v_folio_liquida > 0) THEN
         EXECUTE FUNCTION fn_tab_movimiento (0,v_folio_liquida,"")
                     INTO v_tabla;

         IF v_tabla IS NOT NULL THEN

           LET v_qry = " select FIRST 1 f_liquida,"||
                              " sum(monto_acciones), "||
                              " sum(monto_pesos) "||
                              " from "||v_tabla||
                        " where folio_liquida = "||v_folio_liquida||
                          " and id_derechohabiente = "|| v_id_dh||
                          " group by 1 ";

             PREPARE prp_obt_montos FROM v_qry;
             DECLARE cur_obt_montos CURSOR FOR prp_obt_montos;
             OPEN cur_obt_montos;
             FETCH cur_obt_montos INTO v_f_liquida,v_aivs,v_pesos;
             CLOSE cur_obt_montos;
             FREE cur_obt_montos;

             LET v_f_cargo = v_f_liquida;

             --RETURN v_f_liquida,v_aivs,v_pesos;


            IF (v_f_liquida IS NOT NULL) OR
               (v_aivs IS NOT NULL) OR
               (v_pesos IS NOT NULL) THEN
            ELSE
               LET bnd_sin_datos = 1;
            END IF

               IF (v_entidad = 1) THEN

                  IF (v_estado = 140) AND (v_edo_procesar = 120) THEN
                     LET v_estatus = "conciliado";
                  END IF

                  IF (v_estado = 140) AND (v_edo_procesar < 120) THEN
                     LET v_estatus = "por conciliar";
                  END IF

                  IF (v_estado = 20) THEN
                     LET v_estatus = "sin adelanto";
                  END IF
               ELSE
                  LET v_estatus = "conciliado";
                {
                  EXECUTE FUNCTION fn_cred_liq_viv (v_id_dh,0)
                              INTO v_resultado,
                                   v_tpo_orig,
                                   v_tpo_cred,
                                   v_num_cred,
                                   v_f_otor,
                                   v_f_liq_cred,
                                   v_tpo_dscto;
               }
               END IF
         ELSE
            LET bnd_sin_datos = 1;
         END IF
      else
            LET bnd_sin_datos = 1;
      END IF
   END IF

    EXECUTE FUNCTION fn_cred_liq_viv (v_id_dh,0)
                              INTO v_resultado,
                                   v_tpo_orig,
                                   v_tpo_cred,
                                   v_num_cred,
                                   v_f_otor,
                                   v_f_liq_cred,
                                   v_tpo_dscto;

   IF v_cnt_reg >= 1 THEN
   IF bnd_sin_datos = 1 THEN
      if (v_tpo_originacion = 1) OR
         (v_tpo_originacion = 4) THEN

         foreach
            select tabla
              into v_tabla
              from cat_tab_movimiento

            drop table if exists tmp_cnt_monto;

            LET v_qry = "select count(*) cuenta "||
              " from "||v_tabla||" m, "||
                 "  cat_mov_cre c "||
              "where id_derechohabiente = "||v_id_dh||
              " and m.movimiento = c.movimiento "||
              " and id_origen in (1,4) "||
              " and f_liquida >= "||v_f_otorga||
              " into temp tmp_cnt_monto ";

             EXECUTE IMMEDIATE v_qry;
            let v_cnt_monto = 0;

            select *
              into v_cnt_monto
              from tmp_cnt_monto;

            if v_cnt_monto >= 1 then

            let v_qry = null;

            drop table if exists tmp_monto;

             LET v_qry = "select first 1 f_liquida, "||
                  " sum(monto_pesos), "||
                  " sum(monto_acciones) "||
              " from "||v_tabla||" m, "||
                 "  cat_mov_cre c "||
             "where id_derechohabiente = "||v_id_dh||
              " and m.movimiento = c.movimiento "||
              " and id_origen in (1,4) "||
              " and f_liquida >= "||v_f_otorga||
             " group by f_liquida "||
             " order by f_liquida "||
             "into temp tmp_monto ";
            
             EXECUTE IMMEDIATE v_qry;

               select *
                 into v_f_liquida,v_aivs,v_pesos
                 from tmp_monto;

            if (v_pesos >= 1) or 
               (v_aivs >= 1)  then
               exit foreach;
            end if

         end if

         end foreach

      else
         IF v_f_liq_cred is not null then

            foreach
               select tabla
                 into v_tabla
                 from cat_tab_movimiento

            drop table if exists tmp_cnt_monto;

            LET v_qry = "select count(*) cuenta "||
              " from "||v_tabla||" m, "||
                 "  cat_mov_cre c "||
             "where id_derechohabiente = "||v_id_dh||
              " and m.movimiento = c.movimiento "||
              " and id_origen = 2 "||
              " and f_liquida >= "||v_f_otorga||
              " and f_liquida <= "||v_f_liq_cred||
              " into temp tmp_cnt_monto ";

             EXECUTE IMMEDIATE v_qry;
            let v_cnt_monto = 0;

            select *
              into v_cnt_monto
              from tmp_cnt_monto;

            if v_cnt_monto >= 1 then

            let v_qry = null;

            drop table if exists tmp_monto;

            LET v_qry = "select first 1 f_liquida, "||
                  " sum(monto_pesos), "||
                  " sum(monto_acciones) "||
              " from "||v_tabla||" m, "||
                 "  cat_mov_cre c "||
             "where id_derechohabiente = "||v_id_dh||
              " and m.movimiento = c.movimiento "||
              " and id_origen = 2 "||
              " and f_liquida >= "||v_f_otorga||
              " and f_liquida <= "||v_f_liq_cred||
             " group by f_liquida "||
             " order by f_liquida "||
             " into temp tmp_monto ";

             EXECUTE IMMEDIATE v_qry;

               select *
                 into v_f_liquida,v_aivs,v_pesos
                 from tmp_monto;

               if (v_pesos >= 1) or
                  (v_aivs >= 1)  then
                  exit foreach;
               end if
           end if
 
           end foreach

         ELSE

            foreach
               select tabla
                 into v_tabla
                 from cat_tab_movimiento

            drop table if exists tmp_cnt_monto;

            LET v_qry = "select count(*) cuenta "||
              " from "||v_tabla||" m, "||
                 "  cat_mov_cre c "||
               "where id_derechohabiente = "||v_id_dh||
              " and m.movimiento = c.movimiento "||
              " and id_origen = 2 "||
              " and f_liquida >= "||v_f_otorga||
              " into temp tmp_cnt_monto ";

             EXECUTE IMMEDIATE v_qry;

            LET v_cnt_monto = 0;

            select *
              into v_cnt_monto
              from tmp_cnt_monto;

            if v_cnt_monto >= 1 then

            let v_qry = null;

            drop table if exists tmp_monto;

            LET v_qry = "select first 1 f_liquida, "||
                  " sum(monto_pesos), "||
                  " sum(monto_acciones) "||
              " from "||v_tabla||" m, "||
                 "  cat_mov_cre c "||
             "where id_derechohabiente = "||v_id_dh||
              " and m.movimiento = c.movimiento "||
              " and id_origen = 2 "||
              " and f_liquida >= "||v_f_otorga||
             " group by f_liquida "||
             " order by f_liquida "||
             " INTO TEMP tmp_monto ";

             EXECUTE IMMEDIATE v_qry;

               select *
                 into v_f_liquida,v_aivs,v_pesos
                 from tmp_monto;

               if (v_pesos >= 1) or
                  (v_aivs >= 1)  then
                  exit foreach;
               end if

             end if
            end foreach
         END IF
      end if
   ELSE
         LET v_num_credito = v_num_credito;
         LET v_aivs        = 0;   
         LET v_pesos       = 0;   
         LET v_tpo_credito = v_tpo_credito;
         LET v_f_cargo     = NULL;
         LET v_estatus     = "sin adelanto";
         LET v_f_liquida   = v_f_liq_cred;
   END IF
else
         LET v_num_credito = v_num_credito;
         LET v_aivs        = 0;
         LET v_pesos       = 0;
         LET v_tpo_credito = v_tpo_credito;
         LET v_f_cargo     = NULL;
         LET v_estatus     = "sin adelanto";
         LET v_f_liquida   = v_f_liq_cred;

end if

   IF (v_aivs IS NULL) THEN
       LET v_aivs = 0;
   END IF
   
   IF (v_pesos IS NULL) THEN
       LET v_pesos = 0;
   END IF

    if v_estatus is null then
       let v_estatus = "sin adelanto";
    end if

    RETURN v_error,v_num_credito,v_aivs,v_pesos,v_tpo_credito,v_f_cargo,v_estatus,v_f_liq_cred;
END FUNCTION;


