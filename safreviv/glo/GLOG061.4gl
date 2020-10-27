IMPORT com
###############################################################################
#PROYECTO          => SAFRE VIVIENDA                                          #
#PROPIETARIO       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#MODULO            =>GLO                                                      #
#PROGRAMA          =>GLOG06                                                   #
#OBJETIVO          => REGISTRO DE BITACORA PARA IDENTIFICADOR                 #
#                     DE ID  PARA SERVICIOS WEB                               #
#FECHA INICIO      => 24 OCTUBRE DE 2020                                      #
#AUTOR             => JAIRO GIOVANNY PALAFOX SANCHEZ                          #
###############################################################################
DATABASE safre_viv

{MAIN
 DEFINE v_respuesta         STRING
 CALL fn_notifica_bitacora_ws() RETURNING v_respuesta
         

    {  LET v_ParametrosEnvio = '<?xml version="1.0" encoding="UTF-8"?>',
                      '<ns0:MT_EventoSesion xmlns:ns0="http://infonavit.net/EventTracking/message:getMessage">',
                      '<sistemaId/>',
                      '<identificadorId/>',
                      '<sesionId/>',
                      '<url_link/>',
                      '<pagina/>',
                      '<eventos>',
                      '   <eventoId/>',
                      '   <timestamp/>',
                      '</eventos>',
                      '</ns0:MT_EventoSesion>'
    
             
   END MAIN}