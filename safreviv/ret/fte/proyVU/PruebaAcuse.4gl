DATABASE safre_viv

MAIN
DEFINE v_bytes_reporte BYTE
DEFINE v_nss LIKE afi_derechohabiente.nss
DEFINE v_pesos_viv92 LIKE ret_control_vu.saldo_pesos_viv92
DEFINE v_pesos_viv97 LIKE ret_control_vu.saldo_pesos_viv97
DEFINE v_pesos_fa LIKE ret_control_vu.saldo_pesos_fa
DEFINE v_pesos_total LIKE ret_control_vu.saldo_total_pesos
DEFINE v_sello_digital STRING

    DISPLAY "\nInvocando generacion de reporte y obtencion de cadena de bytes"

    DISPLAY "\nGenerando hash-sello digital"
    -- CALL fn_genera_sello_cadena_original_solicitud_retiro_vu("01511905091", 1, "ELCASOCRM", 13437511, 13437512, 13437516, 354159.78)
    CALL fn_genera_sello_cadena_original_solicitud_retiro_vu("01511905091", 1, "ELCASOCRM", 13437511, null, null, 354159.78)
    RETURNING v_sello_digital
    
    CALL fn_genera_acuse_solicitud_retiro_vu("01511905091", "2020-10-30 21:58","ELCASOCRM", "INTERNET", 4100, 50000, 300059.78, 354159.78, "", v_sello_digital)
         RETURNING v_bytes_reporte
         
    CALL v_bytes_reporte.writeFile("d:/pdf_reconstruido_desde_bytes_20201030.pdf")
    
    DISPLAY "\nReporte generado y bytes obtenidos con exito"

END MAIN