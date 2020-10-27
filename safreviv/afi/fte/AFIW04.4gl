################################################################################
# Descripción : Programa Cliente que consume el WS de movimientos aclaratorios #
################################################################################
GLOBALS "AFIW05.inc"

MAIN
   DEFINE v_nss      CHAR(11)
   DEFINE soapStatus		INTEGER

   CLOSE WINDOW SCREEN

   OPEN WINDOW w1 WITH FORM "AFIW041"

   INPUT BY NAME v_nss ATTRIBUTES (UNBUFFERED)

      {ON ACTION ACCEPT
         CALL consulta_afore(v_nss) RETURNING soapStatus,
                                              consulta_aforeResponse.codresp,
                                              consulta_aforeResponse.nss,
                                              consulta_aforeResponse.afore

         DISPLAY consulta_aforeResponse.codresp TO codResp
         DISPLAY consulta_aforeResponse.nss     TO nss_resp
         DISPLAY consulta_aforeResponse.afore   TO afore
}
      ON ACTION ACCEPT 
         CALL consulta_aclaratorio(v_nss) RETURNING soapStatus,
                                               movimientosaclaratoriosResponse.element

         DISPLAY "========================="
         DISPLAY "ELEMENT: \n"
         DISPLAY "========================="
      ON ACTION CANCEL
         EXIT INPUT
   END INPUT
   CLOSE WINDOW w1
END MAIN
