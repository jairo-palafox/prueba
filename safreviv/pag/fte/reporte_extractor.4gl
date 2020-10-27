DATABASE safre_viv
DEFINE v_ruta_1     STRING
DEFINE v_texto STRING
DEFINE v_contador INTEGER
DEFINE v_contador_string STRING

MAIN
    
    CALL fn_imprime_registro()
END MAIN


FUNCTION fn_imprime_registro()
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
    DEFINE v_query_archivo_preca STRING
    DEFINE v_fecha STRING
    DEFINE v_f_pago_string STRING
    DEFINE v_f_ejecucion_string STRING
    DEFINE v_f_informacion_string STRING
    DEFINE v_folio_aux STRING
    DEFINE v_folio_sua_aux STRING
    DEFINE v_imp_pago_aux DECIMAL(12,0)
    DEFINE v_imp_pago_aux_string STRING

    --Se define la ruta de salida del archivo generado
     LET v_ruta_1 = "/safreviv_int/pag/envio/archivo_salida05.txt"

     --Se inicia el reporte
     START REPORT reporte_extractor TO FILE v_ruta_1 WITH LEFT MARGIN = 0, TOP MARGIN = 0, 
          BOTTOM MARGIN = 0
    
    LET v_fecha=TODAY USING "yyyymmdd" CLIPPED
    LET v_fecha="1"||v_fecha||"1                                                                           "--\n                                                                          \n"
    LET v_texto=v_fecha

    --Se imprime encabezado
    OUTPUT TO REPORT reporte_extractor()
    
    LET v_texto=NULL

    --Query principal
    LET v_query_archivo_preca="SELECT * FROM TMP_ARCHIVO_PRECA1"

    PREPARE prp_query_archivo_preca FROM v_query_archivo_preca
    DECLARE cur_query_archivo_preca CURSOR FOR prp_query_archivo_preca

    LET v_contador=0

   --     FOREACH  cur_query_archivo_preca INTO v_f_informacion,v_folio,
    --    v_nss,v_nrp,v_folio_sua,v_f_pago,v_periodo_pago,v_imp_pago,v_f_ejecucion,v_id_concepto

        FOREACH  cur_query_archivo_preca INTO   v_periodo_pago,
                                                v_f_pago,
                                                v_folio_sua,
                                                v_nrp,
                                                v_nss,
                                                v_imp_pago,
                                                v_f_informacion,
                                                v_folio,
                                                v_f_ejecucion,
                                                v_id_concepto

         --No se imprimen encabezados
            IF v_nss==' ' AND v_nrp==' ' THEN
                CONTINUE FOREACH
            END IF

             --No se imprimen registros con nss en detalle, nulo
            IF v_nss==' ' AND v_nrp<>' ' THEN
                CONTINUE FOREACH
            END IF

                    --Se define contador para el sumario
                    LET v_contador=v_contador+1

                    --Se adaptan las variables al layout
                    LET v_folio_aux=v_folio USING "&&&&&&&&&&&&"
                    LET v_folio_sua_aux=v_folio_sua USING "&&&&&&"
                    LET v_imp_pago_aux=v_imp_pago*100
                    LET v_imp_pago_aux_string=v_imp_pago_aux USING "&&&&&&&&&&&&&"
                    LET v_f_informacion_string=v_f_informacion USING "yyyymmdd"
                    LET v_f_pago_string=v_f_pago USING "yyyymmdd"
                    LET v_f_ejecucion_string=v_f_ejecucion USING "yyyymmdd"
                
                    LET v_texto="2"||v_f_informacion_string||v_folio_aux
                    ||v_nss||v_nrp||v_folio_sua_aux||
                    v_f_pago_string||v_periodo_pago||v_imp_pago_aux_string||
                    v_f_ejecucion_string||"1"
                    LET v_texto=v_texto CLIPPED

                    --Se imprime cadena
                    OUTPUT TO REPORT reporte_extractor()
                    LET v_texto=NULL
        END FOREACH

        LET v_contador_string=v_contador USING "&&&&&&&&&&"
        LET v_texto="9"||v_contador_string
        
        OUTPUT TO REPORT reporte_extractor()
        FINISH REPORT reporte_extractor
           

END FUNCTION

REPORT reporte_extractor()
    FORMAT
        ON EVERY ROW
        PRINT v_texto

END REPORT