IMPORT os
IMPORT com

###############################################################################
#Proyecto          => SACI  INFONAVIT                                         #
#Propietario       => Omnisys                                                 #
-------------------------------------------------------------------------------
#Modulo            =>GLO                                                      #
#Programa          =>GLOG06                                                   #
#Objetivo          => Función que va generar el registro de bitacora para     #
#                     los servicios web del Infonavit con otros sistemas      #
#                     internos / externos                                     #
#Fecha Inicio      => 26 Octubre de 2020                                      #
#Autor             => Jairo Giovanny Palafox Sanchez                          #
###############################################################################
GLOBALS
 DEFINE g_ParametrosEnvio   STRING
END GLOBALS
FUNCTION fn_registra_bitacora_ws(p_sistamaId,p_sesionID,p_array_eventos) 
 DEFINE v_sql               CHAR(1000)
 DEFINE p_sistamaId         CHAR(50)
 DEFINE p_sesionID          CHAR(100)
 DEFINE v_identificadorId   CHAR(50)
 DEFINE v_url_link          CHAR(100)
 DEFINE v_pagina            CHAR(100)
 DEFINE v_indice            SMALLINT
 DEFINE v_encabezado        CHAR(100)
 DEFINE v_evento_inicio     CHAR(100)
 DEFINE v_evento_final      CHAR(100)
 DEFINE v_respuesta         STRING
DEFINE p_array_eventos DYNAMIC ARRAY OF RECORD
        eventoId        CHAR(100),
        timestamp       CHAR(50)
       END RECORD
 -- Definicion de variables retorno
 --parametro retorno funcion
 DEFINE v_resultado         SMALLINT
 DEFINE v_cadena_evento     CHAR(500)
 
  -- valor inicial a las variables del programa
  LET v_indice           = 0 
  -- se inician variables de entrada
  LET v_encabezado    = '<?xml version="1.0" encoding="UTF-8"?>'
  LET v_evento_inicio = '<ns0:MT_EventoSesion xmlns:ns0="http://infonavit.net/EventTracking/message:getMessage">' 
  LET v_evento_final  = '</ns0:MT_EventoSesion>'
    
  LET v_resultado        = 0

  -- se obtiene los datos para el identificador de servicio
  LET v_sql = "SELECT  identificadorid, url_link, pagina ",
              "\n FROM ws_ctr_maestra                                        ",
              "\n WHERE id_ws_ctr_maestra = ?                                "

  PREPARE pre_obt_datos_servicio FROM v_sql
  EXECUTE pre_obt_datos_servicio USING p_sistamaId
                                 INTO v_identificadorId  ,
                                      v_url_link         ,
                                      v_pagina           

  DISPLAY "LO DATOS A REGISTRAR BITACORA:>> "
  DISPLAY "===================================================================="
  DISPLAY "SISTEMA ID:       >",p_sistamaId
  DISPLAY "IDENTIFICADOR ID: >",v_identificadorId
  DISPLAY "SESION ID:        >",p_sesionID
  DISPLAY "URL LINK          >",v_url_link
  DISPLAY "PAGINA:           >",v_pagina                                    
  -- para cada uno de los datos ingresados se envia a la bitacora
  -- se arma respuesta para cada uno de los datos del arreglo
  DISPLAY "VALORES DEL ARREGLO 1: > ",p_array_eventos[1].*
  DISPLAY "VALORES DEL ARREGLO 2: > ",p_array_eventos[1].*
  
  -- se arma el detalle de eventos
  CALL fn_arma_eventos(p_array_eventos) RETURNING v_cadena_evento
   -- se arma respuesta
  LET g_ParametrosEnvio =  v_encabezado                         ,
                           v_evento_inicio                      ,
                           "<sistemaId>",p_sistamaId CLIPPED,"<sistemaId/>", 
                           "<identificadorId>",v_identificadorId CLIPPED,"<identificadorId/>", 
                           "<sesionID>",p_sesionID CLIPPED,"<sesionID/>", 
                           "<url_link>", v_url_link CLIPPED,"<url_link/>", 
                           "<pagina>", v_pagina CLIPPED,"<pagina/>",
                           "<eventos>",v_cadena_evento CLIPPED,"<eventos/>",
                           v_evento_final
                           
  
  CALL fn_notifica_bitacora_ws() RETURNING v_respuesta
 
  RETURN v_resultado 

END FUNCTION 
###############################################################################
#Objetivo          => FunciOn que va notificar el registro a la bitacora para #
#                     el control de solicitudes Infonavit y la interaccion con#
#                     otros sistemas internos / externos                      #
#Fecha Inicio      => 26 Octubre de 2020                                      #
#Autor             => Jairo Giovanny Palafox Sanchez                          #
###############################################################################
FUNCTION fn_notifica_bitacora_ws() 
 DEFINE v_RequestWS         com.HttpRequest
 DEFINE v_ResponseWS        com.HttpResponse
 DEFINE v_URL               STRING
 DEFINE v_StatusWS          INTEGER
 DEFINE v_respuesta         STRING
 
   -- se sume que todo se ejecuta sin problemas
   LET v_respuesta = ""

   --CALL ui.Interface.refresh()
   LET v_Respuesta = NULL

   LET v_URL       = "http://091402aq135.infonavit.net:50100/RESTAdapter/getMessageEvent"
   LET v_RequestWS = com.HttpRequest.Create(v_URL)
        
   CALL v_RequestWS.setMethod("POST")
   CALL v_RequestWS.setAuthentication("PIDDEVELOP","Inicio01","Basic","http://091402aq135.infonavit.net:50100/RESTAdapter/getMessageEvent")
   CALL v_RequestWS.setHeader("Content-Type", "application/xml")
   
   -- se notifica para cada una de las cadenas solicitadas
   CALL v_RequestWS.doTextRequest(g_ParametrosEnvio)
        
   LET v_ResponseWS = v_RequestWS.getResponse()
   LET v_StatusWS   = v_ResponseWS.getStatusCode()

   -- se verifica resultado de validacion
   -- el valor de retorno satisfactorio es 202   
   IF  v_StatusWS = 200 THEN
      LET v_Respuesta = v_ResponseWS.getTextResponse()
   ELSE
      LET v_Respuesta = v_StatusWS
   END IF
    
  DISPLAY "Respuesta: ", v_Respuesta

  RETURN v_respuesta
 
END FUNCTION
FUNCTION fn_arma_eventos(p_array_eventos)
 DEFINE v_indice    SMALLINT
 DEFINE p_array_eventos DYNAMIC ARRAY OF RECORD
         eventoId        CHAR(100),
         timestamp       CHAR(50)
        END RECORD
 DEFINE v_cadena_evento   CHAR(5000)
 
  LET v_cadena_evento = ""

  FOR v_indice = 1 TO  p_array_eventos.getLength()
     -- se acumulan eventos
     LET v_cadena_evento = v_cadena_evento CLIPPED ,"<eventoId>",p_array_eventos[v_indice].eventoId  CLIPPED, "</eventoId>" , 
                                                    "<timestamp>",p_array_eventos[v_indice].timestamp CLIPPED,"</timestamp>"  
                       
  END FOR
  DISPLAY "Respuesta Formada: ", v_cadena_evento
 RETURN v_cadena_evento
END FUNCTION
###############################################################################
#Objetivo          => Funcion que va obtener la fecha en formato timestamp    #
#                     para la bitacora de servicios web                       #
#Fecha Inicio      => 28 Octubre de 2020                                      #
#Autor             => Jairo Giovanny Palafox Sanchez                          #
###############################################################################
FUNCTION fn_obt_formato_timestamp()
 DEFINE v_sql          CHAR(100)
 DEFINE r_f_timestamp  CHAR(25)

  -- se asigna un valor inicial
  LET r_f_timestamp = TODAY
  -- se obtiene la fecha en formato timestamp
  LET v_sql = "SELECT current timestamp  FROM systables",
              "\nWHERE tabid = 1                       "
  PREPARE pre_obt_fecha FROM v_sql
  EXECUTE pre_obt_fecha INTO r_f_timestamp
  DISPLAY "FECHA FORMADO TIMESTAMP: ",r_f_timestamp
 RETURN r_f_timestamp
END FUNCTION 
