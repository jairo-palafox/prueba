DATABASE safre_tmp

MAIN
    
    CALL fn_busca_registro()
END MAIN

FUNCTION fn_busca_registro()
    DEFINE v_nss LIKE tmp_extr_cred.nss
    DEFINE v_nrp LIKE tmp_extractor.nrp
    DEFINE v_folio_sua LIKE tmp_extractor.folio_sua
    DEFINE v_f_pago LIKE tmp_extractor.f_pago
    DEFINE v_periodo_pago LIKE tmp_extractor.periodo_pago
    DEFINE v_imp_pago LIKE tmp_extractor.imp_pago
    DEFINE v_existe_en_credito INTEGER
    DEFINE v_query_extractor STRING
    DEFINE v_query_credito STRING

    LET v_query_extractor="SELECT NSS,NRP,FOLIO_SUA,F_PAGO,PERIODO_PAGO,IMP_PAGO 
                           FROM TMP_EXTRACTOR"
    LET v_query_credito="SELECT COUNT(*) FROM TMP_ARC_CREDITO
                         WHERE NSS=? AND NRP=? AND FOLIO_SUA=? 
                         AND F_PAGO=? AND PERIODO_PAGO=? AND IMP_PAGO=?"
                         

    PREPARE prp_query_extractor FROM v_query_extractor
    DECLARE cur_query_extractor CURSOR FOR prp_query_extractor

    PREPARE prp_query_credito FROM v_query_credito

    FOREACH  cur_query_extractor INTO v_nss,v_nrp,v_folio_sua,v_f_pago,v_periodo_pago,v_imp_pago

        EXECUTE prp_query_credito USING v_nss,v_nrp,v_folio_sua,v_f_pago,v_periodo_pago,v_imp_pago
        INTO v_existe_en_credito

            IF v_existe_en_credito==0 THEN
                CALL fn_inserta_registro_archivo()
            END IF
        
    END FOREACH


END FUNCTION

FUNCTION fn_inserta_registro_archivo()
    DISPLAY "ENTRO"

END FUNCTION