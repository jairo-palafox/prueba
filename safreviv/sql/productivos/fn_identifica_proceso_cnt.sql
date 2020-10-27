






CREATE PROCEDURE "safreviv".fn_identifica_proceso_cnt(p_folio_liquida   DECIMAL(9,0), --Folio de liquidación del proceso
                                           p_fecha_liquida   DATE,         --Fecha de liquidación del proceso
                                           p_cve_proceso_cnt SMALLINT,     --Código de proceso contable
                                           p_cve_proceso     SMALLINT,     --Código Proceso
                                           p_transaccion     SMALLINT)     --Código transacción contable

RETURNING SMALLINT;

--Última modificación 16102019
--Declaración de variables
DEFINE r_bnd_proceso_cnt SMALLINT; --Estatus del proceso
LET r_bnd_proceso_cnt = 1; --Estado correcto

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_id_proc_cnt.trace';
   --   TRACE 'Folio Liquida '||p_folio_liquida;
   --   TRACE 'Fecha Liquida '||p_fecha_liquida;
   --   TRACE 'Proceso Contable '||p_cve_proceso_cnt;
   --   TRACE 'Proceso'||p_cve_proceso;
   --   TRACE 'Transaccion Contable '||p_transaccion;

   --#fn_reg_cnt_72_92
   IF p_cve_proceso_cnt =  1  OR p_cve_proceso_cnt =  4 OR
      p_cve_proceso_cnt = 64  OR p_cve_proceso_cnt = 65 OR 
      p_cve_proceso_cnt = 69  OR p_cve_proceso_cnt = 86 OR
      p_cve_proceso_cnt = 116 THEN
      EXECUTE PROCEDURE safre_viv:fn_reg_cnt_72_92(p_folio_liquida,
                                                   p_fecha_liquida,
                                                   p_cve_proceso_cnt,
                                                   p_cve_proceso,
                                                   p_transaccion)
                                              INTO r_bnd_proceso_cnt;
   END IF

   --#fn_ret_72_92_cnt3
   IF p_cve_proceso_cnt =  3  OR p_cve_proceso_cnt = 54 OR 
      p_cve_proceso_cnt = 118 THEN
      EXECUTE PROCEDURE safre_viv:fn_ret_72_92_cnt3(p_folio_liquida,
                                                    p_fecha_liquida,
                                                    p_cve_proceso_cnt,
                                                    p_cve_proceso,
                                                    p_transaccion)
                                               INTO r_bnd_proceso_cnt;
   END IF

   --#fn_ind_sar_92_cnt5
   IF p_cve_proceso_cnt = 5 THEN
      EXECUTE PROCEDURE safre_viv:fn_ind_sar_92_cnt5(p_folio_liquida,
                                                     p_fecha_liquida,
                                                     p_cve_proceso_cnt,
                                                     p_cve_proceso,
                                                     p_transaccion)
                                                INTO r_bnd_proceso_cnt;
   END IF

   --#fn_tra_in_af_92_cnt6
   IF p_cve_proceso_cnt = 6 THEN         
      EXECUTE PROCEDURE safre_viv:fn_tra_in_af_92_cnt6(p_folio_liquida,
                                                       p_fecha_liquida,
                                                       p_cve_proceso_cnt,
                                                       p_cve_proceso,
                                                       p_transaccion)
                                                  INTO r_bnd_proceso_cnt;

      {EXECUTE PROCEDURE safre_viv:fn_reg_rend_cnt1(p_folio_liquida,
                                                   p_fecha_liquida,
                                                   p_cve_proceso_cnt,
                                                   p_cve_proceso,
                                                   p_transaccion)
                                              INTO r_bnd_proceso_cnt;}
   END IF

   --#fn_ret_n_sar_92_cnt8
   IF p_cve_proceso_cnt = 8 THEN
      EXECUTE PROCEDURE safre_viv:fn_ret_n_sar_92_cnt8(p_folio_liquida,
                                                      p_fecha_liquida,
                                                      p_cve_proceso_cnt,
                                                      p_cve_proceso,
                                                      p_transaccion)
                                                 INTO r_bnd_proceso_cnt;
   END IF

   --#fn_reg_rend_cnt
   IF p_cve_proceso_cnt = 7 THEN
      EXECUTE PROCEDURE safre_viv:fn_reg_rend_cnt(p_folio_liquida,
                                                  p_fecha_liquida,
                                                  p_cve_proceso_cnt,
                                                  p_cve_proceso,
                                                  p_transaccion)
                                             INTO r_bnd_proceso_cnt;
   END IF

   --#fn_ret_sar_92_97_cnt14
   IF p_cve_proceso_cnt = 14 OR p_cve_proceso_cnt = 29 OR
      p_cve_proceso_cnt = 49 THEN
      EXECUTE PROCEDURE safre_viv:fn_ret_sar_92_97_cnt14(p_folio_liquida,
                                                         p_fecha_liquida,
                                                         p_cve_proceso_cnt,
                                                         p_cve_proceso,
                                                         p_transaccion)
                                                    INTO r_bnd_proceso_cnt;
   END IF

   --#fn_pag_lq_cnt15
   IF p_cve_proceso_cnt = 15 THEN
   EXECUTE PROCEDURE safre_viv:fn_pag_lq_cnt15(p_folio_liquida,
                                               p_fecha_liquida,
                                               p_cve_proceso_cnt,
                                               p_cve_proceso,
                                               p_transaccion)
                                          INTO r_bnd_proceso_cnt;

   {EXECUTE PROCEDURE safre_viv:fn_reg_rend_cnt1(p_folio_liquida,
                                                p_fecha_liquida,
                                                p_cve_proceso_cnt,
                                                p_cve_proceso,
                                                p_transaccion)
                                           INTO r_bnd_proceso_cnt;}
   END IF

   --#fn_pag_pau_cnt16
   IF p_cve_proceso_cnt = 16 THEN
   EXECUTE PROCEDURE safre_viv:fn_pag_pau_cnt16(p_folio_liquida,
                                                p_fecha_liquida,
                                                p_cve_proceso_cnt,
                                                p_cve_proceso,
                                                p_transaccion)
                                           INTO r_bnd_proceso_cnt;

   {EXECUTE PROCEDURE safre_viv:fn_reg_rend_cnt1(p_folio_liquida,
                                                p_fecha_liquida,
                                                p_cve_proceso_cnt,
                                                p_cve_proceso,
                                                p_transaccion)
                                           INTO r_bnd_proceso_cnt;}
   END IF

   --#fn_aclara_cnt17
   IF p_cve_proceso_cnt = 17 THEN
      EXECUTE PROCEDURE safre_viv:fn_aclara_cnt17(p_folio_liquida,
                                                  p_fecha_liquida,
                                                  p_cve_proceso_cnt,
                                                  p_cve_proceso,
                                                  p_transaccion)
                                             INTO r_bnd_proceso_cnt;
   END IF

   --#fn_avance_cnt18
   IF p_cve_proceso_cnt = 18 THEN
      EXECUTE PROCEDURE safre_viv:fn_avance_cnt18(p_folio_liquida,
                                                  p_fecha_liquida,
                                                  p_cve_proceso_cnt,
                                                  p_cve_proceso,
                                                  p_transaccion)
                                             INTO r_bnd_proceso_cnt;
   END IF

   --#fn_dis_cnt19
   IF p_cve_proceso_cnt = 19 THEN
      EXECUTE PROCEDURE safre_viv:fn_dis_cnt19(p_folio_liquida,
                                               p_fecha_liquida,
                                               p_cve_proceso_cnt,
                                               p_cve_proceso,
                                               p_transaccion)
                                          INTO r_bnd_proceso_cnt;
   END IF

   --#fn_cnt_acr_97
   IF p_cve_proceso_cnt = 20 THEN
      EXECUTE PROCEDURE safre_viv:fn_cnt_acr_97(p_folio_liquida,
                                                  p_fecha_liquida,
                                                  p_cve_proceso_cnt,
                                                  p_cve_proceso,
                                                  p_transaccion)
                                             INTO r_bnd_proceso_cnt;
   END IF

   --#fn_cnt_dev_ssv_97
   IF p_cve_proceso_cnt = 22 THEN
      EXECUTE PROCEDURE safre_viv:fn_cnt_dev_ssv_97(p_folio_liquida,
                                                    p_fecha_liquida,
                                                    p_cve_proceso_cnt,
                                                    p_cve_proceso,
                                                    p_transaccion)
                                               INTO r_bnd_proceso_cnt;
   END IF

   --#fn_dev_err_cnt26
   IF p_cve_proceso_cnt = 26 THEN
      EXECUTE PROCEDURE safre_viv:fn_dev_err_cnt26(p_folio_liquida,
                                                   p_fecha_liquida,
                                                   p_cve_proceso_cnt,
                                                   p_cve_proceso,
                                                   p_transaccion)
                                              INTO r_bnd_proceso_cnt;

      {EXECUTE PROCEDURE safre_viv:fn_reg_rend_cnt1(p_folio_liquida,
                                                   p_fecha_liquida,
                                                   p_cve_proceso_cnt,
                                                   p_cve_proceso,
                                                   p_transaccion)
                                              INTO r_bnd_proceso_cnt;}
   END IF

   --#fn_dev_pat_97_cnt27
   IF p_cve_proceso_cnt = 27 OR p_cve_proceso_cnt = 28 OR 
      p_cve_proceso_cnt = 76 THEN
      EXECUTE PROCEDURE safre_viv:fn_dev_pat_97_cnt27(p_folio_liquida,
                                                      p_fecha_liquida,
                                                      p_cve_proceso_cnt,
                                                      p_cve_proceso,
                                                      p_transaccion)
                                                 INTO r_bnd_proceso_cnt;
   END IF

   --#fn_ret_solo_inf97_cnt30
   IF p_cve_proceso_cnt = 30 OR p_cve_proceso_cnt = 55 THEN
      EXECUTE PROCEDURE safre_viv:fn_ret_solo_inf97_cnt30(p_folio_liquida,
                                                          p_fecha_liquida,
                                                          p_cve_proceso_cnt,
                                                          p_cve_proceso,
                                                          p_transaccion)
                                                     INTO r_bnd_proceso_cnt;
   END IF

   --#fn_ret_tra_jud_cnt31
   IF p_cve_proceso_cnt = 31 THEN
      EXECUTE PROCEDURE safre_viv:fn_ret_tra_jud_cnt31(p_folio_liquida,
                                                       p_fecha_liquida,
                                                       p_cve_proceso_cnt,
                                                       p_cve_proceso,
                                                       p_transaccion)
                                                  INTO r_bnd_proceso_cnt;
   END IF

   --#fn_reg_cnt_97
   IF p_cve_proceso_cnt = 32 OR p_cve_proceso_cnt = 63 THEN
      EXECUTE PROCEDURE safre_viv:fn_reg_cnt_97(p_folio_liquida,
                                                p_fecha_liquida,
                                                p_cve_proceso_cnt,
                                                p_cve_proceso,
                                                p_transaccion)
                                           INTO r_bnd_proceso_cnt;
   END IF

   --#fn_anu_gar_cnt34
   IF p_cve_proceso_cnt = 34 THEN
      EXECUTE PROCEDURE safre_viv:fn_anu_gar_cnt34(p_folio_liquida,
                                                   p_fecha_liquida,
                                                   p_cve_proceso_cnt,
                                                   p_cve_proceso,
                                                   p_transaccion)
                                              INTO r_bnd_proceso_cnt;
   END IF

   --#fn_sep_ctas_cnt36
   IF p_cve_proceso_cnt = 36 OR p_cve_proceso_cnt = 50 THEN
      EXECUTE PROCEDURE safre_viv:fn_sep_ctas_cnt36(p_folio_liquida,
                                                    p_fecha_liquida,
                                                    p_cve_proceso_cnt,
                                                    p_cve_proceso,
                                                    p_transaccion)
                                               INTO r_bnd_proceso_cnt;
   END IF

   --#fn_uni_ctas_cnt37
   IF p_cve_proceso_cnt = 37 OR p_cve_proceso_cnt = 78 OR 
      p_cve_proceso_cnt = 95 OR p_cve_proceso_cnt = 96 THEN
      EXECUTE PROCEDURE safre_viv:fn_uni_ctas_cnt37(p_folio_liquida,
                                                    p_fecha_liquida,
                                                    p_cve_proceso_cnt,
                                                    p_cve_proceso,
                                                    p_transaccion)
                                               INTO r_bnd_proceso_cnt;
   END IF

   --#fn_ret_ley73_gp1_cnt38
   IF p_cve_proceso_cnt = 38 OR p_cve_proceso_cnt = 59 THEN
      EXECUTE PROCEDURE safre_viv:fn_ret_ley73_gp1_cnt38(p_folio_liquida,
                                                         p_fecha_liquida,
                                                         p_cve_proceso_cnt,
                                                         p_cve_proceso,
                                                         p_transaccion)
                                                    INTO r_bnd_proceso_cnt;
   END IF

   --#fn_ret_ley73_gps_cnt39
   IF p_cve_proceso_cnt = 39 OR p_cve_proceso_cnt = 60 THEN
      EXECUTE PROCEDURE safre_viv:fn_ret_ley73_gps_cnt39(p_folio_liquida,
                                                         p_fecha_liquida,
                                                         p_cve_proceso_cnt,
                                                         p_cve_proceso,
                                                         p_transaccion)
                                                    INTO r_bnd_proceso_cnt;
   END IF

   --#fn_fort_credito_cnt
   IF p_cve_proceso_cnt = 42 THEN
      EXECUTE PROCEDURE safre_viv:fn_fort_credito_cnt(p_folio_liquida,
                                                      p_fecha_liquida,
                                                      p_cve_proceso_cnt,
                                                      p_cve_proceso,
                                                      p_transaccion)
                                                 INTO r_bnd_proceso_cnt;
   END IF

   --#fn_fort_credito_cnt
   IF p_cve_proceso_cnt = 43 THEN
      EXECUTE PROCEDURE safre_viv:fn_fort_credito_cnt(p_folio_liquida,
                                                      p_fecha_liquida,
                                                      p_cve_proceso_cnt,
                                                      p_cve_proceso,
                                                      p_transaccion)
                                                 INTO r_bnd_proceso_cnt;
   END IF

   --#fn_avance_cnt18
   IF p_cve_proceso_cnt = 44 THEN
      EXECUTE PROCEDURE safre_viv:fn_avance_cnt18(p_folio_liquida,
                                                  p_fecha_liquida,
                                                  p_cve_proceso_cnt,
                                                  p_cve_proceso,
                                                  p_transaccion)
                                             INTO r_bnd_proceso_cnt;
   END IF

   --#fn_avance_cnt18
   IF p_cve_proceso_cnt = 45 OR p_cve_proceso_cnt = 51 THEN
      EXECUTE PROCEDURE safre_viv:fn_avance_cnt18(p_folio_liquida,
                                                  p_fecha_liquida,
                                                  p_cve_proceso_cnt,
                                                  p_cve_proceso,
                                                  p_transaccion)
                                             INTO r_bnd_proceso_cnt;
   END IF

   --#fn_ret_contingente
   IF p_cve_proceso_cnt = 46  OR p_cve_proceso_cnt = 47  OR
      p_cve_proceso_cnt = 48  OR p_cve_proceso_cnt = 113 OR 
      p_cve_proceso_cnt = 114 THEN
      EXECUTE PROCEDURE safre_viv:fn_ret_contingente(p_folio_liquida,
                                                     p_fecha_liquida,
                                                     p_cve_proceso_cnt,
                                                     p_cve_proceso,
                                                     p_transaccion)
                                                INTO r_bnd_proceso_cnt;
   END IF

   --#fn_apo_vol_cnt52
   IF p_cve_proceso_cnt = 52 OR p_cve_proceso_cnt = 67 OR 
      p_cve_proceso_cnt = 68 THEN
      EXECUTE PROCEDURE safre_viv:fn_apo_vol_cnt52(p_folio_liquida,
                                                   p_fecha_liquida,
                                                   p_cve_proceso_cnt,
                                                   p_cve_proceso,
                                                   p_transaccion)
                                              INTO r_bnd_proceso_cnt;

      {EXECUTE PROCEDURE safre_viv:fn_reg_rend_cnt1(p_folio_liquida,
                                                   p_fecha_liquida,
                                                   p_cve_proceso_cnt,
                                                   p_cve_proceso,
                                                   p_transaccion)
                                              INTO r_bnd_proceso_cnt;}
   END IF

   --#fn_dev_amo_cnt53
   IF p_cve_proceso_cnt = 53 OR p_cve_proceso_cnt = 72 OR
      p_cve_proceso_cnt = 73 OR p_cve_proceso_cnt = 74 OR 
      p_cve_proceso_cnt = 75 OR p_cve_proceso_cnt = 97 OR 
      p_cve_proceso_cnt = 98 THEN
      EXECUTE PROCEDURE safre_viv:fn_dev_amo_cnt53(p_folio_liquida,
                                                   p_fecha_liquida,
                                                   p_cve_proceso_cnt,
                                                   p_cve_proceso,
                                                   p_transaccion)
                                              INTO r_bnd_proceso_cnt;

      {EXECUTE PROCEDURE safre_viv:fn_reg_rend_cnt1(p_folio_liquida,
                                                   p_fecha_liquida,
                                                   p_cve_proceso_cnt,
                                                   p_cve_proceso,
                                                   p_transaccion)
                                              INTO r_bnd_proceso_cnt;}
   END IF

   --#fn_ret_fnd_ah_cnt57
   IF p_cve_proceso_cnt = 57 OR p_cve_proceso_cnt = 58 OR 
      p_cve_proceso_cnt = 70 OR p_cve_proceso_cnt = 71 THEN
      EXECUTE PROCEDURE safre_viv:fn_ret_fnd_ah_cnt57(p_folio_liquida,
                                                     p_fecha_liquida,
                                                     p_cve_proceso_cnt,
                                                     p_cve_proceso,
                                                     p_transaccion)
                                                INTO r_bnd_proceso_cnt;
   END IF

   --#fn_ret_ley73_gp4_cnt61
   IF p_cve_proceso_cnt = 61 OR p_cve_proceso_cnt = 62 THEN
      EXECUTE PROCEDURE safre_viv:fn_ret_ley73_gp4_cnt61(p_folio_liquida,
                                                         p_fecha_liquida,
                                                         p_cve_proceso_cnt,
                                                         p_cve_proceso,
                                                         p_transaccion)
                                                    INTO r_bnd_proceso_cnt;
   END IF

   --#fn_pag_mdt_cnt66
   IF p_cve_proceso_cnt = 66 OR p_cve_proceso_cnt = 80 THEN
      EXECUTE PROCEDURE safre_viv:fn_pag_mdt_cnt66(p_folio_liquida,
                                                   p_fecha_liquida,
                                                   p_cve_proceso_cnt,
                                                   p_cve_proceso,
                                                   p_transaccion)
                                              INTO r_bnd_proceso_cnt;
   END IF

   --#fn_ajuste_op_cnt
   IF p_cve_proceso_cnt = 79 THEN
      EXECUTE PROCEDURE safre_viv:fn_ajuste_op_cnt(p_folio_liquida,
                                                   p_fecha_liquida,
                                                   p_cve_proceso_cnt,
                                                   p_cve_proceso,
                                                   p_transaccion)
                                              INTO r_bnd_proceso_cnt;
   END IF
   
   -- fn_prt_cnt: 81, 82, 83, 84, 85, 87, 88 , 90 y 94
   IF p_cve_proceso_cnt = 81 OR p_cve_proceso_cnt = 82 OR 
      p_cve_proceso_cnt = 83 OR p_cve_proceso_cnt = 84 OR
      p_cve_proceso_cnt = 85 OR p_cve_proceso_cnt = 87 OR
      p_cve_proceso_cnt = 88 OR p_cve_proceso_cnt = 90 OR 
	  p_cve_proceso_cnt = 94 THEN
	  
      EXECUTE PROCEDURE safre_viv:fn_prt_cnt(p_folio_liquida,
                                             p_fecha_liquida,
                                             p_cve_proceso_cnt,
                                             p_cve_proceso,
                                             p_transaccion)
                                        INTO r_bnd_proceso_cnt;
   END IF

   --#fn_ret_ley73_cnt
   IF p_cve_proceso_cnt = 91 OR p_cve_proceso_cnt = 92 THEN
      EXECUTE PROCEDURE safre_viv:fn_ret_ley73_cnt(p_folio_liquida,
                                                   p_fecha_liquida,
                                                   p_cve_proceso_cnt,
                                                   p_cve_proceso,
                                                   p_transaccion)
                                              INTO r_bnd_proceso_cnt;
   END IF

   --#fn_ajuste_sdo_tesofe_cnt
   IF p_cve_proceso_cnt = 99  OR p_cve_proceso_cnt = 101 OR 
      p_cve_proceso_cnt = 102 THEN
      EXECUTE PROCEDURE safre_viv:fn_ajuste_sdo_tesofe_cnt(p_folio_liquida,
                                                           p_fecha_liquida,
                                                           p_cve_proceso_cnt,
                                                           p_cve_proceso,
                                                           p_transaccion)
                                                      INTO r_bnd_proceso_cnt;
   END IF

   --#fn_cargos_ssv_cnt
   IF p_cve_proceso_cnt = 100 THEN
      EXECUTE PROCEDURE safre_viv:fn_cargos_ssv_cnt(p_folio_liquida,
                                                    p_fecha_liquida,
                                                    p_cve_proceso_cnt,
                                                    p_cve_proceso,
                                                    p_transaccion)
                                               INTO r_bnd_proceso_cnt;
   END IF

   --#fn_exc_dev_ssv_cnt
   IF p_cve_proceso_cnt = 103 THEN
      EXECUTE PROCEDURE safre_viv:fn_exc_dev_ssv_cnt(p_folio_liquida,
                                                     p_fecha_liquida,
                                                     p_cve_proceso_cnt,
                                                     p_cve_proceso,
                                                     p_transaccion)
                                                INTO r_bnd_proceso_cnt;
   END IF

   --#fn_rest_ret_ley73_cnt
   IF p_cve_proceso_cnt = 105 OR 
      p_cve_proceso_cnt = 115 OR 
      p_cve_proceso_cnt = 117 THEN
      EXECUTE PROCEDURE safre_viv:fn_rest_ret_ley73_cnt(p_folio_liquida,
                                                        p_fecha_liquida,
                                                        p_cve_proceso_cnt,
                                                        p_cve_proceso,
                                                        p_transaccion)
                                                   INTO r_bnd_proceso_cnt;
   END IF

   --#fn_reg_pag_cambiavit_cnt
   IF p_cve_proceso_cnt = 106 THEN
      EXECUTE PROCEDURE safre_viv:fn_reg_pag_cambiavit_cnt(p_folio_liquida,
                                                           p_fecha_liquida,
                                                           p_cve_proceso_cnt,
                                                           p_cve_proceso,
                                                           p_transaccion)
                                                      INTO r_bnd_proceso_cnt;
   END IF

   --#fn_reg_pag_cambiavit_cnt
   IF p_cve_proceso_cnt = 107 THEN
      EXECUTE PROCEDURE safre_viv:fn_reg_pag_cambiavit_cnt(p_folio_liquida,
                                                           p_fecha_liquida,
                                                           p_cve_proceso_cnt,
                                                           p_cve_proceso,
                                                           p_transaccion)
                                                      INTO r_bnd_proceso_cnt;
   END IF

   --#fn_reg_pag_cambiavit_cnt
   IF p_cve_proceso_cnt = 108 THEN
      EXECUTE PROCEDURE safre_viv:fn_reg_pag_cambiavit_cnt(p_folio_liquida,
                                                           p_fecha_liquida,
                                                           p_cve_proceso_cnt,
                                                           p_cve_proceso,
                                                           p_transaccion)
                                                      INTO r_bnd_proceso_cnt;
   END IF

   --#fn_reg_pag_cambiavit_cnt
   IF p_cve_proceso_cnt = 109 THEN
      EXECUTE PROCEDURE safre_viv:fn_reg_pag_cambiavit_cnt(p_folio_liquida,
                                                           p_fecha_liquida,
                                                           p_cve_proceso_cnt,
                                                           p_cve_proceso,
                                                           p_transaccion)
                                                      INTO r_bnd_proceso_cnt;
   END IF

   --#fn_avance_cnt18
   IF p_cve_proceso_cnt = 112 THEN
      EXECUTE PROCEDURE safre_viv:fn_avance_cnt18(p_folio_liquida,
                                                  p_fecha_liquida,
                                                  p_cve_proceso_cnt,
                                                  p_cve_proceso,
                                                  p_transaccion)
                                             INTO r_bnd_proceso_cnt;
   END IF

   --TRACE 'sp_identifica_proceso_cnt';
   --TRACE 'Bandera de retorno'||r_bnd_proceso_cnt;

   RETURN r_bnd_proceso_cnt;

END PROCEDURE;


