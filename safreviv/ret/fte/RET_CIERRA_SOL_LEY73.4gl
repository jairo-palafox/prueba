#
# FOURJS_START_COPYRIGHT(U,2000)
# PROPERTY OF FOUR JS*
# (C) COPYRIGHT FOUR JS 2000, 2011. ALL RIGHTS RESERVED.
# * TRADEMARK OF FOUR JS DEVELOPMENT TOOLS EUROPE LTD
#   IN THE UNITED STATES AND ELSEWHERE
# 
# FOUR JS AND ITS SUPPLIERS DO NOT WARRANT OR GUARANTEE THAT THESE SAMPLES
# ARE ACCURATE AND SUITABLE FOR YOUR PURPOSES. THEIR INCLUSION IS PURELY 
# FOR INFORMATION PURPOSES ONLY.
# FOURJS_END_COPYRIGHT
#

GLOBALS "ws_retiroLey73cierre.inc"


MAIN
  DEFINE nss          CHAR(11)           -- NSS
  DEFINE cod_rechazo  SMALLINT           -- GRUPO DE TRABAJADOR 

DEFINE v_nss             CHAR(11)     ,--Número de seguridad social del trabajador
       v_cod_retorno     SMALLINT     ,--Código de retorno   Según cátalogo a dos posiciones   Numérico 
       v_mensaje_retorno VARCHAR(255)    ,--Mensaje   Mensaje de código de retorno    Texto 
       cbx_cod_rechazo   ui.ComboBox
         
  DEFINE wsstatus     INTEGER  
     
  OPEN WINDOW w1 WITH FORM "RET_CIERRA_SOL_LEY73" ATTRIBUTE (TEXT="Cierra Solicitud Retiro Ley 73")
  
    INPUT BY NAME 
     nss,
     cod_rechazo
    ATTRIBUTE (UNBUFFERED, WITHOUT DEFAULTS)

       BEFORE INPUT
          -- se llena el combo de codigos de cierre/rechazo
          LET cbx_cod_rechazo = ui.ComboBox.forName("formonly.cod_rechazo")

          CALL cbx_cod_rechazo.clear()

          -- se llena el combop
          CALL cbx_cod_rechazo.addItem(1,"Cierre por parte de CECI")
          CALL cbx_cod_rechazo.addItem(2,"Cierre por documentación incorrecta")
          CALL cbx_cod_rechazo.addItem(3,"Trabajador no acepta montos")
          CALL cbx_cod_rechazo.addItem(4,"Trabajador no se presentó a cita")

       -- se invoca la funcion para cierre
       ON ACTION ACCEPT
          CALL fn_cierra_solicitud_retiro_ley73(nss, cod_rechazo) 
               RETURNING v_nss,
                         v_cod_retorno,
                         v_mensaje_retorno,
                         wsstatus

          DISPLAY v_nss,
                  v_cod_retorno,
                  v_mensaje_retorno
          TO nss_respuesta,
             cod_retorno,
             mensaje_retorno

     ON ACTION cancel
       EXIT INPUT       
    END INPUT 

  CLOSE WINDOW w1
  
END MAIN

