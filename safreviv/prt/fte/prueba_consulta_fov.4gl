GLOBALS "PRTWS01.inc"


MAIN
DEFINE r_resultado_invoca_ws INTEGER
   CALL consultaCreditoFovissste(3,
                                 "12332112312",
                                 "Martínez",
                                 "Perez",
                                 "Dayana", 
                                 "MAPD12345678912345",
                                 43
                                 ) RETURNING r_resultado_invoca_ws,
                                             v_msj_salida_solicitud_cedente.v_folio_transaccion,
                                             v_msj_salida_solicitud_cedente.v_nss,
                                             v_msj_salida_solicitud_cedente.v_ap_paterno,
                                             v_msj_salida_solicitud_cedente.v_ap_materno,
                                             v_msj_salida_solicitud_cedente.v_nombre,
                                             v_msj_salida_solicitud_cedente.v_curp,
                                             v_msj_salida_solicitud_cedente.v_id_credito,
                                             v_msj_salida_solicitud_cedente.v_resultado_operacion,
                                             v_msj_salida_solicitud_cedente.v_id_motivo,
                                             v_msj_salida_solicitud_cedente.v_descripcion,
                                             v_msj_salida_solicitud_cedente.v_f_originacion_credito,
                                             v_msj_salida_solicitud_cedente.v_saldo_insoluto_credito

   DISPLAY "Resultado:",r_resultado_invoca_ws
   DISPLAY v_msj_salida_solicitud_cedente.*

END MAIN