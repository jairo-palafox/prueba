


MAIN
   DEFINE v_comando                       STRING

   DISPLAY ""
   DISPLAY "*******************************************************************"
   DISPLAY " Inicia el proceso para cargar los NSS "
   DISPLAY " "
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"
   DISPLAY ""

   LET v_comando = "dbaccess safre_tmp /safreviv_req/SACI2018-4/carga_pendientes.sql"
   RUN v_comando

   LET v_comando = "dbaccess safre_viv /safreviv_req/SACI2018-4/respalda_curp.sql"
   RUN v_comando

   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Termino la carga de los NSS's: "
   DISPLAY ""
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY ""
   DISPLAY "*******************************************************************"
   DISPLAY ""
   
   DISPLAY ""
   DISPLAY "*******************************************************************"
   DISPLAY " Inicia el proceso para actualizar la CURP"
   DISPLAY " "
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"
   DISPLAY ""
   DATABASE safre_viv
   
   UPDATE afi_derechohabiente SET curp = NULL WHERE nss IN (SELECT nss FROM safre_tmp:tmp_nss)

   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "El proceso para actualizar la CURP finalizo correctamente "
   DISPLAY ""
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY ""
   DISPLAY "*******************************************************************"
   DISPLAY ""
END MAIN