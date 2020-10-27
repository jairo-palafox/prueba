DATABASE safre_viv

DEFINE v_texto STRING
DEFINE channel_1    base.Channel
DEFINE v_ruta_1     STRING

MAIN
    
    CALL fn_busca_registro()
END MAIN

FUNCTION fn_busca_registro()

    DEFINE v_nss CHAR(11)
    DEFINE v_f_ejecucion DATE
    DEFINE v_f_informacion DATE
    DEFINE v_f_pago DATE
    DEFINE v_folio DECIMAL(9,0)
    DEFINE v_folio_sua DECIMAL(6,0)
    DEFINE v_id_concepto CHAR(1)
    DEFINE v_imp_pago DECIMAL(12,2)
    DEFINE v_nrp CHAR(11)
    DEFINE v_periodo_pago CHAR(6)
    
    DEFINE v_existe_en_credito INTEGER
    DEFINE v_query_extractor STRING
    DEFINE v_query_credito STRING
    DEFINE v_bandera INTEGER
    DEFINE v_fecha STRING

    DEFINE v_f_pago_string STRING
    DEFINE v_f_ejecucion_string STRING
    DEFINE v_f_informacion_string STRING

    DEFINE v_folio_aux STRING
    DEFINE v_folio_sua_aux STRING
    DEFINE v_imp_pago_aux DECIMAL(12,0)
    DEFINE v_imp_pago_aux_string STRING

    LET v_ruta_1 = "/safreviv_int/pag/envio/archivo_salida.txt"
    LET channel_1 = base.Channel.create()
    CALL channel_1.openFile(v_ruta_1,"w")
    CALL channel_1.setDelimiter("")

    LET v_query_extractor="SELECT *  
                           FROM TMP_EXTRACTOR_TOTAL"
    LET v_query_credito="SELECT COUNT(*) FROM TMP_ARCHIVO_CREDITO
                         WHERE NSS=? AND NRP=? AND FOLIO_SUA=? 
                         AND F_PAGO=? AND PERIODO_PAGO=? AND IMP_PAGO=?"
                         
    LET v_fecha=TODAY USING "yyyymmdd"
    
    LET v_fecha="1"||v_fecha||"1                                                                           "--\n                                                                          \n"

    LET v_texto=v_fecha
    CALL channel_1.write(v_texto)
    LET v_texto=NULL


    PREPARE prp_query_extractor FROM v_query_extractor
    DECLARE cur_query_extractor CURSOR FOR prp_query_extractor

    FOREACH  cur_query_extractor INTO v_f_informacion,v_folio,
    v_nss,v_nrp,v_folio_sua,v_f_pago,v_periodo_pago,v_imp_pago,v_f_ejecucion,v_id_concepto

    PREPARE prp_query_credito FROM v_query_credito
        EXECUTE prp_query_credito USING v_nss,v_nrp,
                v_folio_sua,v_f_pago,v_periodo_pago,
                v_imp_pago
                INTO v_existe_en_credito
      
            IF v_existe_en_credito==0 THEN
                    LET v_folio_aux=v_folio USING "&&&&&&&&&&&&"
                    LET v_folio_sua_aux=v_folio_sua USING "&&&&&&"
                    LET v_imp_pago_aux=v_imp_pago
                    LET v_imp_pago_aux_string=v_imp_pago_aux USING "&&&&&&&&&&&&&"
                    LET v_f_informacion_string=v_f_informacion USING "yyyymmdd"
                    LET v_f_pago_string=v_f_pago USING "yyyymmdd"
                    LET v_f_ejecucion_string=v_f_ejecucion USING "yyyymmdd"
                
                    LET v_texto="2"||v_f_informacion_string||v_folio_aux
                    ||v_nss||v_nrp||v_folio_sua_aux||
                    v_f_pago_string||v_periodo_pago||v_imp_pago_aux_string||
                    v_f_ejecucion_string||"1"--||"\n"
                    CALL channel_1.write(v_texto)
                    LET v_texto=NULL                
            END IF
    END FOREACH
    CALL channel_1.close()
 {   IF v_bandera==1 THEN
        CALL fn_inserta_registro_archivo()
        
    END IF}

   -- CALL fn_inserta_registro_archivo()

END FUNCTION
{
FUNCTION fn_inserta_registro_archivo()
    DEFINE channel_1    base.Channel

    DEFINE v_ruta_1     STRING
        
       -- LET v_ruta_1 = "/ds/safreviv/pag/bin/archivo_salida.txt"
        LET v_ruta_1 = "/safreviv_int/pag/extra/archivo_salida.txt"
      -- DISPLAY v_ruta_1
        LET channel_1 = base.Channel.create()

         CALL channel_1.openFile(v_ruta_1,"w")
         CALL channel_1.setDelimiter("")

         CALL channel_1.write(v_texto)
         CALL channel_1.close()
END FUNCTION}



