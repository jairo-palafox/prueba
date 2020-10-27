--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 22/10/2015
--==============================================================================

################################################################################
#Modulo       => GLO                                                           #
#Programa     =>                                                               #
#Objetivo     => Libreria para consulta de servicios activos                   #
#Fecha inicio => 22 Octubre 2015                                               #
################################################################################
IMPORT com

FUNCTION fn_consulta_estado_ws(p_url_servicio)
DEFINE p_url_servicio STRING,
       v_peticion     com.HTTPRequest,
       v_respuesta    com.HTTPResponse,
       v_conteo_enc   SMALLINT,
       v_indice       SMALLINT,
       v_servicio_activo BOOLEAN,
       v_wsdl            STRING

   LET v_servicio_activo = FALSE
   INITIALIZE v_wsdl TO NULL

   # URL del servicio a consultar
   LET p_url_servicio = p_url_servicio||"?WSDL"
   --DISPLAY p_url_servicio
   TRY 
      # Crea Request de la petición
      LET v_peticion = com.HTTPRequest.Create(p_url_servicio)
      # Idica que se requerirá la definición del servicio
      CALL v_peticion.setMethod("GET")
      --CALL v_peticion.setMethod("HEAD")
      CALL v_peticion.setHeader("SOAPAction","\"\"")
      # Reliza petición al servicio
      CALL v_peticion.doRequest()
      # Procesa respuesta
      LET v_respuesta = v_peticion.getResponse()
      IF( v_respuesta.getStatusCode() != 200 )THEN
         DISPLAY  "HTTP Error ("||v_respuesta.getStatusCode()||") ",v_respuesta.getStatusDescription()
         LET v_servicio_activo = FALSE
      ELSE 
         LET v_servicio_activo = TRUE
         LET v_wsdl = v_respuesta.getTextResponse()
         {LET v_conteo_enc = v_respuesta.getHeaderCount()         
         FOR v_indice = 1 TO v_conteo_enc
            DISPLAY v_respuesta.getHeaderName(v_indice)||" = "|| v_respuesta.getHeaderValue(v_indice)
         END FOR}
      END IF 
   CATCH
      --DISPLAY "ERROR :",STATUS||" ("||SQLCA.SQLERRM||")"
      LET v_servicio_activo = FALSE 
   END TRY

   RETURN v_servicio_activo,
          v_wsdl
END FUNCTION