
DATABASE safre_viv


MAIN
    DISPLAY "PASO"
   -- CALL fn_busca_registro()
END MAIN

FUNCTION fn_busca_registro()
    DEFINE v_nss STRING
    DEFINE v_nrp STRING
    DEFINE v_folio_sua STRING
    DEFINE v_f_pago STRING
    DEFINE v_periodo_pago STRING
    DEFINE v_imp_pago STRING
    DEFINE v_existe_en_credito INTEGER
    DEFINE v_query_extractor STRING
    DEFINE v_query_credito STRING

    LET v_query_extractor="SELECT NSS,NRP,FOLIO_SUA,F_PAGO,PERIODO_PAGO,IMP_PAGO 
                           FROM SAFRE_TMP:TMP_EXTRACTOR"
  --  LET v_query_credito="SELECT COUNT(*) FROM TMP_ARC_CREDITO
   --                      WHERE NSS=? AND NRP=? AND FOLIO_SUA=? 
   --                      AND F_PAGO=? AND PERIODO_PAGO=? AND IMP_PAGO=?"

    LET v_query_credito="SELECT COUNT(*) FROM AFI_DECRETO"
                         

 {   PREPARE prp_query_extractor FROM v_query_extractor
    DECLARE cur_query_extractor CURSOR FOR prp_query_extractor}

    PREPARE prp_query_credito FROM v_query_credito
    EXECUTE prp_query_credito INTO v_existe_en_credito
    
  {  DISPLAY v_existe_en_credito

    FOREACH  cur_query_extractor --INTO v_nss,v_nrp,v_folio_sua,v_f_pago,v_periodo_pago,v_imp_pago

        EXECUTE prp_query_credito-- USING v_nss,v_nrp,v_folio_sua,v_f_pago,v_periodo_pago,v_imp_pago
        INTO v_existe_en_credito

        DISPLAY v_existe_en_credito

            IF v_existe_en_credito==0 THEN
                CALL fn_inserta_registro_archivo()
            END IF
        
    END FOREACH}


END FUNCTION

FUNCTION fn_inserta_registro_archivo()
    DISPLAY "ENTRO"

END FUNCTION