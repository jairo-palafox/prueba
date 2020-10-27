####################################################################
#Modulo            =>NOT                                           #
#Programa          =>NOTW04                                        #
#Objetivo          =>Define la función que va a explotar el web    #
#                    service enviarNotificaciones                  #
#Fecha inicio      =>13 NOVIEMBRE 2016                             #
####################################################################

GLOBALS "NOTW01.inc"
GLOBALS "NOTW02.inc"



FUNCTION fn_envia_notificacion(v_in)
    DEFINE v_in                     tDT_CargarCampana_in  
    DEFINE v_out                    tDT_CargarCampana_out 
    DEFINE v_respuesta_wc           INTEGER--Va a contener la respuesta del WC

    LET SI_CargarCampana_SOService_HTTP_PortEndpoint.Address.Uri = "alias://enviarNotificacion"
     --Se iguala la variable de entrada con la variable del ws
    LET MT_CargarCampana_req.* = v_in.*
    INITIALIZE MT_CargarCampana_res TO NULL
    INITIALIZE v_out.* TO NULL
    INITIALIZE wsErr.* TO NULL
    
    --Se ejecuta la función del cliente
   CALL SI_CargarCampana_SO_g() RETURNING v_respuesta_wc

   --DISPLAY  "código de respuesta",v_respuesta_wc
   IF v_respuesta_wc <> 0 THEN 
      LET v_out.codigoRetorno = NULL
      LET  v_out.descripcion="Ocurrio un error al transmitir el archivo..." 
      DISPLAY " "
      DISPLAY "wsError.action: ", wsError.action
      DISPLAY "wsError.code: ", wsError.code
      DISPLAY "wsError.codeNS: ", wsError.codeNS
      DISPLAY "wsError.description: ", wsError.description
   ELSE 
      LET v_out.* = MT_CargarCampana_res.*
   END IF 
   LET wsErr.* = wsError.*

   RETURN v_out.*

END FUNCTION 